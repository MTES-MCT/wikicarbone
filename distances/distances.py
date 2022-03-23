import json
import requests
import pandas as pd
import itertools
import geopy.distance
import json
import os


def getSearatesDistance(route_type, route):
    """Query the Searates API for a route ("FR","CN") and a route_type ("road") and returns the distance

    Args:
        route_type (string): "road", "sea" or "air"
        route (tuple): Pair of countries alpha 2 codes : ("FR","CN")

    Returns:
        float : distance of the route in km for the given route_type
    """
    url = buildSearatesQuery(route_type, route)
    response = requests.get(url)
    resp_json = response.json()
    try:
        dist = round(float(resp_json[route_type]["dist"]))
    except KeyError:
        dist = None
    return dist


def buildSearatesQuery(route_type, route):
    """build the url to query the searates API based on a route ("FR","CN") and a route_type ("road")

    Args:
        route_type (string): "road", "sea" or "air"
        route (tuple): Pair of countries alpha 2 codes : ("FR","CN")

    Returns:
        string: url to query the searates API
    """

    base_url = "https://sirius.searates.com/distance-and-time/search?type=" + route_type
    coords0 = country_coords[route[0]]
    coords1 = country_coords[route[1]]
    from_str = "&speed=800&lat_from=" + str(coords0[0]) + "&lng_from=" + str(coords0[1])
    to_str = "&lat_to=" + str(coords1[0]) + "&lng_to=" + str(coords1[1])
    countries_str = "&from_country_code=" + route[0] + "&to_country_code=" + route[1]

    return base_url + from_str + to_str + countries_str


df = pd.read_csv("distances/countries_importance.csv")
# select only most important countries
# df = df[(df.importance == 1)]

# build dic of country -> coordinates
country_coords = {}

for i, x in df.iterrows():
    country_coords[x["Alpha-2 code"]] = (
        float(x["Latitude (average)"]),
        float(x["Longitude (average)"]),
    )


# select list of countries to calculate distances
# be careful, the number of pairs of n countries is big : n(n-1)/2

# countries = list(df["Alpha-2 code"])[0:3]
countries = [
    "TR",
    "TN",
    "PT",
    "FR",
    "ES",
    "CN",
    "BD",
    "VN",
    "MA",
    "MM",
    "KH",
    "IT",
    "PK",
    "DE",
]

distances = {}
remaining_countries = countries.copy()
n = len(countries)
nb_routes = round(n * (n - 1) / 2)
print("number of routes : " + str(nb_routes))

for country_from in countries:
    # remove current country (country_from) from remaining_countries
    remaining_countries.remove(country_from)
    # for current country build a dictionary of distances with all remaining countries
    country_from_dic = {}

    if len(remaining_countries) > 0:
        # iterate on all remaining countries (country_to)
        for country_to in remaining_countries:
            route = (country_from, country_to)
            print("computing distances for route " + str(route))

            # get distances between country_from and country_to

            country_from_dic[country_to] = {
                "road": getSearatesDistance("road", route),
                "sea": getSearatesDistance("sea", route),
                "air": round(
                    geopy.distance.distance(
                        country_coords[route[0]], country_coords[route[1]]
                    ).km
                ),
            }
        # add dictionary of distances to master dictionary
        distances[country_from] = country_from_dic
        print("finished computing distances for " + country_from)

with open("distances/distances.json", "w") as outfile:
    json.dump(distances, outfile)
