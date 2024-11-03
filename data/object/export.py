#!/usr/bin/env python
# coding: utf-8

"""Export des processes pour les objets"""

import json
import os
import sys
from os.path import abspath, dirname

from bw2data.project import projects
from common.export import (
    compute_impacts,
    display_changes,
    export_json,
    load_json,
    with_corrected_impacts,
)
from common.impacts import impacts as impacts_py
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
DEFAULT_DB = "Ecoinvent 3.9.1"

with open(CONFIG["IMPACTS_FILE"]) as f:
    IMPACTS_DEF_ECOBALYSE = json.load(f)


# Configure logger
logger.remove()  # Remove default handler
logger.add(sys.stderr, format="{time} {level} {message}", level="INFO")


def create_process_list(activities):
    logger.info("Creating process list...")
    return frozendict({activity["id"]: activity for activity in activities})


if __name__ == "__main__":
    logger.info("Starting export process")
    projects.set_current(CONFIG["PROJECT"])
    # Load activities
    activities = load_json(CONFIG["ACTIVITIES_FILE"])

    # Create process list
    processes = create_process_list(activities)

    # Compute impacts
    processes_impacts = compute_impacts(processes, DEFAULT_DB, impacts_py)

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