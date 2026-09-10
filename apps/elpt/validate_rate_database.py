"""Validate a refreshed JSON database before GitHub commits it; no packages needed."""

import json
import sys
from pathlib import Path

from update_urdb_rates import CUSTOM_RATES_FILE, OUTPUT_FILE, eia_utility_ids, load_custom_rates


def validate_database(path):
    with Path(path).open(encoding="utf-8") as source:
        rates = json.load(source)
    if not isinstance(rates, list) or not rates:
        raise ValueError("The rate database must be a nonempty JSON array.")

    seen_ids = set()
    utility_ids = set()
    by_id = {}
    for rate in rates:
        if not isinstance(rate, dict) or any(
            not rate.get(field)
            for field in ("OpenEI_ID", "EIA_Utility_ID", "Utility_Name", "Rate_Name")
        ):
            raise ValueError("A rate is missing its ID, utility, or name.")
        rate_id = rate["OpenEI_ID"]
        if rate_id in seen_ids:
            raise ValueError(f"Duplicate OpenEI_ID: {rate_id}")
        seen_ids.add(rate_id)
        utility_ids.add(str(rate["EIA_Utility_ID"]))
        by_id[rate_id] = rate

    custom_rates = load_custom_rates(CUSTOM_RATES_FILE)
    allowed_ids = set(eia_utility_ids) | {str(rate["EIA_Utility_ID"]) for rate in custom_rates}
    if utility_ids - allowed_ids:
        raise ValueError("The database contains an unconfigured utility ID.")
    for custom_rate in custom_rates:
        if by_id.get(custom_rate["OpenEI_ID"]) != custom_rate:
            raise ValueError(f"Custom rate was changed or omitted: {custom_rate['OpenEI_ID']}")
    return len(rates)


if __name__ == "__main__":
    try:
        count = validate_database(sys.argv[1] if len(sys.argv) > 1 else OUTPUT_FILE)
    except (OSError, ValueError) as error:
        raise SystemExit(f"Rate database validation failed: {error}") from error
    print(f"Validated {count} rates, utility identities, and all persistent custom rates.")
