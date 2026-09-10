"""Quarterly URDB refresh calendar (Python standard library only)."""

from datetime import datetime
from zoneinfo import ZoneInfo


def is_refresh_day(day):
    return day.month in (1, 4, 7, 10) and day.weekday() == 0 and 8 <= day.day <= 14


if __name__ == "__main__":
    today = datetime.now(ZoneInfo("America/Los_Angeles")).date()
    print("true" if is_refresh_day(today) else "false")
