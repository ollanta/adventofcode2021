#!/bin/sh
set -euo pipefail


jq "[.members[] | {
  name: .name,
  stars: .completion_day_level[\"$2\"] | objects | map_values(.get_star_ts | todate)
}]" $1
