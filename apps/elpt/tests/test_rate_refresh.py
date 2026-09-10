"""Offline regression checks for the scheduled refresh; no third-party packages."""

import importlib.util
import json
from datetime import date, timedelta
from pathlib import Path
import unittest
from unittest.mock import patch

APP_DIR = Path(__file__).resolve().parents[1]


def load_module(name, filename):
    spec = importlib.util.spec_from_file_location(name, APP_DIR / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


schedule = load_module("refresh_schedule", "refresh_schedule.py")
updater = load_module("update_urdb_rates", "update_urdb_rates.py")


class RateRefreshTests(unittest.TestCase):
    def test_next_refresh_dates(self):
        day = date(2026, 9, 10)
        dates = []
        while len(dates) < 5:
            if schedule.is_refresh_day(day):
                dates.append(day.isoformat())
            day += timedelta(days=1)
        self.assertEqual(dates, [
            "2026-10-12", "2027-01-11", "2027-04-12", "2027-07-12", "2027-10-11"
        ])
        self.assertFalse(schedule.is_refresh_day(date(2026, 10, 5)))

    def test_exactly_one_refresh_per_quarter(self):
        for year in range(2026, 2041):
            for month in (1, 4, 7, 10):
                dates = [date(year, month, day) for day in range(1, 29)]
                self.assertEqual(sum(schedule.is_refresh_day(day) for day in dates), 1)

    @patch.object(updater.time, "sleep")
    @patch.object(updater, "urlopen")
    def test_failed_request_is_reported(self, request, sleep):
        request.side_effect = updater.URLError("offline test")
        rates, failures = updater.fetch_latest_rates(["123"], "test-key")
        self.assertEqual(rates, [])
        self.assertEqual(failures, ["123"])

    @patch.object(updater.time, "sleep")
    @patch.object(updater, "urlopen")
    def test_previously_empty_utility_can_remain_empty(self, request, sleep):
        request.return_value.__enter__.return_value.read.return_value = b'{"items": []}'
        rates, failures = updater.fetch_latest_rates(["123"], "test-key")
        self.assertEqual(rates, [])
        self.assertEqual(failures, [])

    def test_previously_available_utility_cannot_disappear(self):
        previous = [{"EIA_Utility_ID": updater.eia_utility_ids[0]}]
        with self.assertRaises(ValueError):
            updater.validate_utility_coverage([], previous)
        updater.validate_utility_coverage(previous, previous)

    @patch.object(updater.time, "sleep")
    @patch.object(updater, "urlopen")
    def test_active_rate_and_custom_rates_survive(self, request, sleep):
        payload = {"items": [{
            "label": "test-rate", "utility": "Test Utility", "name": "Test C&I",
            "sector": "Industrial", "energyratestructure": [[{"rate": 0.1}]]
        }]}
        request.return_value.__enter__.return_value.read.return_value = json.dumps(payload).encode()
        rates, failures = updater.fetch_latest_rates(["123"], "test-key")
        self.assertEqual(failures, [])
        self.assertEqual(rates[0]["OpenEI_ID"], "test-rate")
        custom = updater.load_custom_rates(updater.CUSTOM_RATES_FILE)
        self.assertIn("LBNL-HYBRID-315000-V1", [rate["OpenEI_ID"] for rate in custom])


if __name__ == "__main__":
    unittest.main()
