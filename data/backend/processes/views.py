from django.conf import settings
from django.contrib.auth import authenticate, login
from django.core.exceptions import PermissionDenied
from django.http import response, JsonResponse
from django.shortcuts import render, redirect

# from django.shortcuts import resolve_url
from django.utils.translation import gettext_lazy as _
from authentication.views import is_token_valid
import json
import logging
import os

# logger = logging.getLogger(__name__)


PUBLIC_FOLDER = "public/data/"


# Pre-load processes files.
with open(os.path.join(PUBLIC_FOLDER, "food/processes.json"), "r") as f:
    food_processes = f.read()

with open(os.path.join(PUBLIC_FOLDER, "textile/processes.json"), "r") as f:
    textile_processes = f.read()

with open(
        os.path.join(PUBLIC_FOLDER, "food/processes_impacts.json"),
        "r") as f:
    food_processes_detailed = f.read()

with open(
        os.path.join(PUBLIC_FOLDER, "textile/processes_impacts.json"),
        "r") as f:
    textile_processes_detailed = f.read()


def processes(request):
    token = request.headers.get("token")
    if token:
        if is_token_valid(token):
            return JsonResponse({
                "foodProcesses": food_processes_detailed,
                "textileProcesses": textile_processes_detailed,
            })
        else:
            return JsonResponse(
                {"error": _("This token isn't valid")},
                status=401,
            )
    else:
        return JsonResponse({
            "foodProcesses": food_processes,
            "textileProcesses": textile_processes,
        })
