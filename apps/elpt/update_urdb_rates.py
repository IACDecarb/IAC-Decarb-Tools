import os
import json
import sys
import time
from datetime import datetime
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import urlopen

# ==========================================
# CONFIGURATION
# ==========================================
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILE = os.path.join(SCRIPT_DIR, '.env')
CUSTOM_RATES_FILE = os.path.join(
    SCRIPT_DIR, 'AllUploadFiles_ToolTesting', 'custom_elpt_rates.json'
)
OUTPUT_FILE = os.path.join(
    SCRIPT_DIR, 'AllUploadFiles_ToolTesting', 'local_database_rates.json'
)


def load_local_env(path='.env'):
    """Load simple KEY=VALUE entries without requiring python-dotenv."""
    if not os.path.exists(path):
        return

    with open(path, encoding='utf-8') as env_file:
        for raw_line in env_file:
            line = raw_line.strip()
            if not line or line.startswith('#') or '=' not in line:
                continue
            key, value = line.split('=', 1)
            key = key.strip()
            value = value.strip().strip('"').strip("'")
            if key:
                os.environ.setdefault(key, value)


load_local_env(ENV_FILE)

# Now your script can find the key!
API_KEY = os.environ.get('NREL_API_KEY')
# Using v8 of the API for the most up-to-date structure
BASE_URL = 'https://api.openei.org/utility_rates'

# Load your 188 utilities (you can also load this from a CSV)
# The utility IDs are kept here so this updater has no spreadsheet dependency.
eia_utility_ids = ['17609', '14328', '6452', '13374', '4226', '5416', '4110',
                   '19327', '7140', '15477', '19876', '66101', '14354', '3046',
                   '4254', '15466', '54913', '6455', '5109', '195', '14006',
                   '14715', '13573', '13781', '14940', '19436', '11804', '56697',
                   '16609', '733', '11241', '15500', '803', '14063', '9726',
                   '11171', '13511', '1167', '814', '20847', '12341', '13407',
                   '4176', '13998', '15248', '15470', '16572', '17539', '15847',
                   '6458', '9191', '11208', '10171', '16604', '18454', '55722',
                   '64289', '12825', '9417', '17718', '17698', '3755', '15472',
                   '9324', '3266', '15270', '12685', '16534', '55878', '15474',
                   '18193', '64682', '61526', '10000', '5027', '3542', '963',
                   '20856', '61432', '5487', '13214', '12796', '54820', '4922',
                   '13756', '55937', '9617', '9273', '61475', '56692', '15473',
                   '15263', '20860', '22500', '1015', '59808', '57132', '11249',
                   '16868', '5701', '14626', '17166', '14127', '3249', '56571',
                   '7601', '20169', '58957', '7279', '20659', '13216', '10005',
                   '3989', '3265', '13780', '57350', '12293', '24211', '12698',
                   '16183', '59799', '59793', '18997', '12470', '56212', '17470',
                   '19107', '12686', '14154', '19547', '19497', '14610', '22053',
                   '59126', '17543', '59385', '20885', '60181', '12745', '14232',
                   '64928', '5860', '60402', '7554', '61858', '16949', '1179',
                   '64425', '60759', '10857', '12647', '3408', '12199', '10421',
                   '5078', '19898', '3916', '18304', '9601', '9094', '18429',
                   '3757', '9216', '65649', '11479', '19159', '61431', '13478',
                   '16865', '64314', '16840', '18642', '15296', '18995', '61131',
                   '13640', '19325', '4410', '12087', '20413', '88888', '59931',
                   '14624', '27000', '9964', '20521', '50046',
                   '16655']


# Define the sectors you want to pull
sectors = ['Commercial', 'Industrial']


def build_fixed_cbl_hybrid_rate(config):
    """Expand a compact planning-rate definition into ELPT's URDB schema."""
    cbl_kwh = float(config.get('CBL_kWh', 0))
    reference_demand_kw = float(config.get('Reference_Billing_Demand_kW', 0))
    fuel_surcharge = float(config.get('CBL_Fuel_Surcharge_Per_kWh', 0))
    incremental_rates = config.get('Incremental_Rates_By_Month')

    if cbl_kwh <= 0 or reference_demand_kw <= 0:
        raise ValueError('Custom CBL energy and reference demand must be positive.')
    if fuel_surcharge < 0:
        raise ValueError('The custom CBL fuel surcharge cannot be negative.')
    if not isinstance(incremental_rates, list) or len(incremental_rates) != 12:
        raise ValueError('Incremental_Rates_By_Month must contain January through December.')
    try:
        incremental_rates = [float(rate) for rate in incremental_rates]
    except (TypeError, ValueError) as error:
        raise ValueError('Every monthly incremental rate must be numeric.') from error
    if any(rate < 0 for rate in incremental_rates):
        raise ValueError('Monthly incremental rates cannot be negative.')

    # Resolve Georgia Power's PLM hours-use-demand blocks once using the
    # explicitly chosen reference demand. The generated tariff then contains
    # ordinary cumulative-kWh tiers and never reads demand from the load file.
    base_tiers = []
    last_bound = 0.0

    def append_tier(upper_bound, rate):
        nonlocal last_bound
        upper_bound = min(float(upper_bound), cbl_kwh)
        if upper_bound > last_bound:
            base_tiers.append({
                'max': int(upper_bound) if upper_bound.is_integer() else upper_bound,
                'unit': 'kWh',
                'rate': rate,
                'adj': 0
            })
            last_bound = upper_bound

    first_block_cap = min(200 * reference_demand_kw, cbl_kwh)
    for upper_bound, rate in (
        (3000, 0.153054),
        (10000, 0.140178),
        (200000, 0.120861),
        (first_block_cap, 0.093761)
    ):
        append_tier(min(upper_bound, first_block_cap), rate + fuel_surcharge)
        if last_bound >= first_block_cap:
            break

    for upper_bound, rate in (
        (400 * reference_demand_kw, 0.015555),
        (600 * reference_demand_kw, 0.011705),
        (cbl_kwh, 0.010177)
    ):
        append_tier(upper_bound, rate + fuel_surcharge)
        if last_bound >= cbl_kwh:
            break

    energy_structure = []
    for incremental_rate in incremental_rates:
        month_tiers = [dict(tier) for tier in base_tiers]
        month_tiers.append({
            'max': None,
            'unit': 'kWh',
            'rate': incremental_rate,
            'adj': 0
        })
        energy_structure.append(month_tiers)

    monthly_schedule = [[month] * 24 for month in range(12)]
    return {
        'EIA_Utility_ID': config.get('EIA_Utility_ID', 'LBNL-CUSTOM'),
        'Utility_Name': config['Utility_Name'],
        'Rate_Name': config['Rate_Name'],
        'Rate_Description': config.get(
            'Rate_Description',
            'Planning-only fixed-CBL hybrid energy rate.'
        ),
        'Basic_Information_Comments': config.get(
            'Basic_Information_Comments',
            'The CBL and reference billing demand are fixed planning assumptions.'
        ),
        'Energy_Comments': config.get(
            'Energy_Comments',
            'Usage above the fixed monthly CBL receives a calendar-month price.'
        ),
        'Service_Type': 'Bundled',
        'OpenEI_ID': config['OpenEI_ID'],
        'OpenEI_URI': None,
        'Source_URL': None,
        'Sector': config.get('Sector', 'Industrial'),
        'Effective_Date': None,
        'End_Date_Status': 'Custom Planning Rate',
        'Fixed_Monthly_Charge': 0,
        'Fixed_Charge_Units': '$/month',
        'Energy_Weekday_Schedule': monthly_schedule,
        'Energy_Weekend_Schedule': monthly_schedule,
        'Energy_Rate_Structure': energy_structure,
        'Demand_Weekday_Schedule': None,
        'Demand_Weekend_Schedule': None,
        'Demand_Rate_Structure': None,
        'FlatDemandStructure': None,
        'FlatDemandMonths': None,
        'Last_Updated_By_URDB': None,
        'Custom_Rate_Type': config['Custom_Rate_Type'],
        'Custom_Rate_Version': config.get('Custom_Rate_Version'),
        'CBL_kWh': cbl_kwh,
        'CBL_Fuel_Surcharge_Per_kWh': fuel_surcharge,
        'Reference_Billing_Demand_kW': reference_demand_kw
    }


def load_custom_rates(path):
    """Load persistent ELPT-only rates that are appended after every refresh."""
    if not os.path.exists(path):
        return []

    with open(path, encoding='utf-8') as custom_file:
        custom_rates = json.load(custom_file)

    if not isinstance(custom_rates, list):
        raise ValueError('custom_elpt_rates.json must contain a JSON array.')

    required_fields = {'Utility_Name', 'Rate_Name', 'OpenEI_ID'}
    seen_ids = set()
    for custom_rate in custom_rates:
        if not isinstance(custom_rate, dict):
            raise ValueError('Every custom rate must be a JSON object.')
        missing = required_fields.difference(custom_rate)
        if missing:
            raise ValueError(
                'A custom rate is missing required fields: '
                + ', '.join(sorted(missing))
            )
        rate_id = custom_rate['OpenEI_ID']
        if rate_id in seen_ids:
            raise ValueError(f'Duplicate custom OpenEI_ID: {rate_id}')
        seen_ids.add(rate_id)

    expanded_rates = []
    for custom_rate in custom_rates:
        if custom_rate.get('Custom_Rate_Type') == 'fixed_cbl_hybrid':
            expanded_rates.append(build_fixed_cbl_hybrid_rate(custom_rate))
        else:
            expanded_rates.append(custom_rate)

    return expanded_rates

# ==========================================
# EXTRACTION FUNCTION
# ==========================================
def fetch_latest_rates(eia_ids, api_key, url="https://api.openei.org/utility_rates"):
    """
    Fetches all active Commercial and Industrial utility rates from the OpenEI URDB API
    for a given list of EIA Utility IDs, including utilities with more than 500 records.
    """
    extracted_rates = []
    request_failures = []

    for eia_id in eia_ids:
        print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] Fetching rates for EIA ID: {eia_id}...")
        current_timestamp = int(time.time())

        # Ask OpenEI for rates effective today, then paginate until it returns
        # fewer than its documented 500-record maximum.
        params = {
            'version': 8,
            'format': 'json',
            'api_key': api_key,
            'eia': eia_id,
            'detail': 'full',
            'limit': 500,
            'effective_on_date': current_timestamp,
            'orderby': 'startdate',
            'direction': 'desc'
        }

        try:
            all_items = []
            offset = 0
            while True:
                page_params = dict(params, offset=offset)
                request_url = f"{url}?{urlencode(page_params)}"
                with urlopen(request_url, timeout=90) as response:
                    data = json.loads(response.read().decode('utf-8'))

                if data.get('errors'):
                    raise ValueError(f"OpenEI API error: {data['errors']}")

                page_items = data.get('items') or []
                all_items.extend(page_items)
                if len(page_items) < params['limit']:
                    break
                offset += len(page_items)
                time.sleep(0.25)

            if all_items:
                print(f"  -> OpenEI returned {len(all_items)} records effective today. Filtering...")

                active_count = 0

                for item in all_items:
                    rate_name = item.get('name', '')
                    rate_sector = item.get('sector', '')

                    # 1. Sector Filter: Skip explicit residential or lighting rates
                    if rate_sector == 'Residential' or rate_sector == 'Lighting':
                        continue

                    # 2. Cleanup Filter: Remove agricultural/pumping rates misclassified as Commercial
                    clean_name = rate_name.lower()
                    if 'agriculture' in clean_name or 'ag-' in clean_name or 'pump' in clean_name:
                        continue

                    # 3. Date Filter Logic: Keep only rates active right now
                    start_date = item.get('startdate')
                    end_date = item.get('enddate')

                    is_expired = end_date is not None and end_date < current_timestamp
                    is_future = start_date is not None and start_date > current_timestamp

                    if not is_expired and not is_future:
                        active_count += 1

                        display_end_date = 'Active (Open-Ended)' if end_date is None else 'Active (Scheduled Expiration)'

                        # 4. Map data perfectly into the flat schema your R Shiny app expects
                        rate_info = {
                            'EIA_Utility_ID': str(eia_id),
                            'Utility_Name': item.get('utility'),
                            'Rate_Name': rate_name,
                            # Retain descriptive metadata so ELPT can identify
                            # dynamic/day-ahead rates even when the rate code
                            # itself does not contain "RTP" or "RTD".
                            'Rate_Description': item.get('description'),
                            'Basic_Information_Comments': item.get('basicinformationcomments'),
                            'Energy_Comments': item.get('energycomments'),
                            'Service_Type': item.get('servicetype'),
                            'OpenEI_ID': item.get('label'),
                            'OpenEI_URI': item.get('uri'),
                            'Source_URL': item.get('source'),
                            'Sector': rate_sector,
                            'Effective_Date': start_date,
                            'End_Date_Status': display_end_date,
                            'Fixed_Monthly_Charge': item.get(
                                'fixedchargefirstmeter', item.get('fixedcharge', 0)
                            ),
                            'Fixed_Charge_Units': item.get('fixedchargeunits'),

                            # 12x24 Schedule Matrices for ELPT hourly load shifting calculation
                            'Energy_Weekday_Schedule': item.get('energyweekdayschedule'),
                            'Energy_Weekend_Schedule': item.get('energyweekendschedule'),
                            'Energy_Rate_Structure': item.get('energyratestructure'),

                            # Peak Demand $/kW Matrices
                            'Demand_Weekday_Schedule': item.get('demandweekdayschedule'),
                            'Demand_Weekend_Schedule': item.get('demandweekendschedule'),
                            'Demand_Rate_Structure': item.get('demandratestructure'),
                            'FlatDemandStructure': item.get('flatdemandstructure'),
                            'FlatDemandMonths': item.get('flatdemandmonths'),

                            'Last_Updated_By_URDB': datetime.now().strftime('%Y-%m-%d')
                        }
                        extracted_rates.append(rate_info)

                print(f"  -> Successfully kept {active_count} active Commercial/Industrial rates.")

            else:
                print(f"  -> No data records found at all for EIA: {eia_id}")

        except (HTTPError, URLError, TimeoutError, json.JSONDecodeError, ValueError) as e:
            print(f"  -> API Request Network/Protocol Error for EIA {eia_id}: {e}")
            request_failures.append(str(eia_id))

        # Courteous pause to respect OpenEI server bandwidth constraints
        time.sleep(1)

    return extracted_rates, request_failures


def validate_utility_coverage(latest_rates, previous_rates):
    """Reject lost utilities while allowing configured IDs that have no C&I rates."""
    configured = set(eia_utility_ids)
    previous_ids = {str(rate.get('EIA_Utility_ID')) for rate in previous_rates} & configured
    refreshed_ids = {str(rate.get('EIA_Utility_ID')) for rate in latest_rates}
    missing = previous_ids - refreshed_ids
    if missing:
        raise ValueError(
            'Previously available utilities returned no active rates: '
            + ', '.join(sorted(missing))
        )


# ==========================================
# EXECUTION AND EXPORT
# ==========================================
if __name__ == "__main__":
    print("Starting URDB Rate Synchronization Pipeline...")

    if not API_KEY:
        print("NREL_API_KEY is missing. Add it to apps/elpt/.env before running this updater.")
        sys.exit(1)

    try:
        custom_rates = load_custom_rates(CUSTOM_RATES_FILE)
    except (OSError, json.JSONDecodeError, ValueError) as error:
        print(f"Custom rate validation failed: {error}")
        sys.exit(1)

    # Run the extraction
    latest_rates, failed_utility_ids = fetch_latest_rates(
        eia_utility_ids, API_KEY, BASE_URL
    )

    if failed_utility_ids:
        print(
            "\nPipeline stopped without replacing the existing JSON because "
            f"{len(failed_utility_ids)} utility request(s) failed: "
            + ", ".join(failed_utility_ids)
        )
        sys.exit(1)

    if latest_rates:
        try:
            with open(OUTPUT_FILE, encoding='utf-8') as previous_file:
                validate_utility_coverage(latest_rates, json.load(previous_file))
        except FileNotFoundError:
            pass  # Allow the first database build.
        except (OSError, ValueError) as error:
            print(f'Pipeline stopped without replacing the existing JSON: {error}')
            sys.exit(1)

        database_ids = {rate.get('OpenEI_ID') for rate in latest_rates}
        duplicate_custom_ids = sorted(
            rate['OpenEI_ID'] for rate in custom_rates
            if rate['OpenEI_ID'] in database_ids
        )
        if duplicate_custom_ids:
            print(
                "Custom rate IDs conflict with OpenEI records: "
                + ", ".join(duplicate_custom_ids)
            )
            sys.exit(1)

        latest_rates.extend(custom_rates)
        if custom_rates:
            print(f"Added {len(custom_rates)} persistent ELPT custom rate(s).")

        # Write completely before atomically replacing the database so a failed
        # update cannot leave the Shiny app with a truncated JSON file.
        output_file = OUTPUT_FILE
        temporary_file = output_file + '.tmp'
        with open(temporary_file, 'w', encoding='utf-8') as json_file:
            json.dump(latest_rates, json_file, indent=4, ensure_ascii=False)
        os.replace(temporary_file, output_file)
        print(
            f"\nPipeline Complete! {len(latest_rates)} rate structures successfully updated and saved to {os.path.relpath(output_file, SCRIPT_DIR)}.")
    else:
        print("\nPipeline finished, but no data was extracted. Check your EIA IDs and API key.")
        sys.exit(1)
