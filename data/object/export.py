#!/usr/bin/env python
# coding: utf-8

"""Export des processes pour les objets"""

import json
import os
import sys
import urllib.parse
from os.path import abspath, dirname

import bw2calc
import requests
from bw2data.project import projects
from common.export import (
    cached_search,
    display_changes,
    export_json,
    load_json,
    progress_bar,
    spproject,
    with_corrected_impacts,
    with_subimpacts,
)
from common.impacts import bytrigram, main_method
from common.impacts import impacts as definitions
from frozendict import frozendict
from loguru import logger

# Add the 'data' directory to the Python path
PROJECT_ROOT_DIR = dirname(dirname(dirname(abspath(__file__))))
DATA_DIR = os.path.join(PROJECT_ROOT_DIR, "data")
sys.path.append(DATA_DIR)

ECOBALYSE_DATA_DIR = os.environ.get("ECOBALYSE_DATA_DIR")
if not ECOBALYSE_DATA_DIR:
    print(
        "\n🚨 ERROR: For the export to work properly, you need to specify ECOBALYSE_DATA_DIR env variable. It needs to point to the https://github.com/MTES-MCT/ecobalyse-private/ repository. Please, edit your .env file accordingly."
    )
    sys.exit(1)

# Configuration
CONFIG = {
    "PROJECT": "default",
    "ACTIVITIES_FILE": f"{PROJECT_ROOT_DIR}/data/object/activities.json",
    "PROCESSES_FILE": f"{ECOBALYSE_DATA_DIR}/data/object/processes_impacts.json",
    "IMPACTS_FILE": f"{PROJECT_ROOT_DIR}/public/data/impacts.json",
    "ECOINVENT": "Ecoinvent 3.9.1",
}

with open(CONFIG["IMPACTS_FILE"]) as f:
    IMPACTS_DEF_ECOBALYSE = json.load(f)


# Configure logger
logger.remove()  # Remove default handler
logger.add(sys.stderr, format="{time} {level} {message}", level="INFO")


def create_process_list(activities):
    logger.info("Creating process list...")
    return frozendict({activity["id"]: activity for activity in activities})


def compute_simapro_impacts(activity, method):
    strprocess = urllib.parse.quote(activity["name"], encoding=None, errors=None)
    project = urllib.parse.quote(spproject(activity), encoding=None, errors=None)
    method = urllib.parse.quote(main_method, encoding=None, errors=None)

    api_request = f"http://simapro.ecobalyse.fr:8000/impact?process={strprocess}&project={project}&method={method}"
    logger.debug(f"SimaPro API request: {api_request}")
    return bytrigram(
        definitions,
        json.loads(requests.get(api_request).content),
    )


def compute_brightway_impacts(activity, method):
    results = dict()
    lca = bw2calc.LCA({activity: 1})
    lca.lci()
    for key, method in definitions.items():
        lca.switch_method(method)
        lca.lcia()
        results[key] = float("{:.10g}".format(lca.score))
        logger.debug(f"{activity}  {key}: {lca.score}")
    return results


def compute_impacts(processes_fd):
    processes = dict(processes_fd)
    logger.info(f"Computing impacts for {len(processes)} processes:")
    for index, (_, process) in enumerate(processes.items()):
        progress_bar(index, len(processes))

        activity = cached_search(
            process.get("database", CONFIG["ECOINVENT"]), process["name"]
        )

        results = compute_brightway_impacts(activity, main_method)

        if process["unit"] == "kilowatt hour" and isinstance(results, dict):
            results = {k: v * 3.6 for k, v in results.items()}
        if process["unit"] == "litre" and isinstance(results, dict):
            results = {k: v / 1000 for k, v in results.items()}

        process["impacts"] = results

        if isinstance(results, dict) and results:
            logger.info(f"Got impacts from Brightway for: {process['name']}")
        else:
            logger.warning(f"Failed to get impacts for: {process['name']}")

        process["impacts"] = with_subimpacts(process["impacts"])

    return frozendict({k: frozendict(v) for k, v in processes.items()})


if __name__ == "__main__":
    logger.info("Starting export process")
    projects.set_current(CONFIG["PROJECT"])
    # Load activities
    activities = load_json(CONFIG["ACTIVITIES_FILE"])

    # Create process list
    processes = create_process_list(activities)

    # Compute impacts
    processes_impacts = compute_impacts(processes)

    # Apply corrections
    processes_corrected_impacts = with_corrected_impacts(
        IMPACTS_DEF_ECOBALYSE, processes_impacts
    )

    # Load old processes for comparison
    oldprocesses = load_json(CONFIG["PROCESSES_FILE"])

    # Display changes
    display_changes("id", oldprocesses, processes_corrected_impacts)

    # Export results
    export_json(list(processes_corrected_impacts.values()), CONFIG["PROCESSES_FILE"])

    logger.info("Export completed successfully.")
