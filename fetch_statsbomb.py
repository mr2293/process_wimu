import pandas as pd
import streamlit as st
from statsbombpy import sb
from datetime import date

CREDS = {"user": "echazaro@clubamerica.com.mx", "passwd": "gXrDvovd"}

COMPETITIONS = {
    "Liga MX": 73,
    "MLS":     44,
}

SEASONS = {
    "2025/2026": 318,
    "2026":      316,
}

st.title("StatsBomb Event Fetcher")

col1, col2 = st.columns(2)
with col1:
    competition_name = st.selectbox("Competition", list(COMPETITIONS.keys()))
with col2:
    season_name = st.selectbox("Season", list(SEASONS.keys()))

competition_id = COMPETITIONS[competition_name]
season_id      = SEASONS[season_name]


@st.cache_data(show_spinner="Loading teams...")
def get_teams(competition_id: int, season_id: int) -> list[str]:
    matches = sb.matches(competition_id=competition_id, season_id=season_id, creds=CREDS)
    teams = sorted(set(matches["home_team"].tolist() + matches["away_team"].tolist()))
    return teams


teams = get_teams(competition_id, season_id)
team  = st.selectbox("Team", teams)

col3, col4 = st.columns(2)
with col3:
    start_date = st.date_input("Start date", value=date(2026, 1, 1))
with col4:
    end_date = st.date_input("End date", value=date.today())

if st.button("Fetch events", type="primary"):
    with st.spinner("Fetching matches..."):
        matches = sb.matches(competition_id=competition_id, season_id=season_id, creds=CREDS)
        matches = matches[(matches["home_team"] == team) | (matches["away_team"] == team)]
        matches = matches[matches["match_status"] == "available"]
        matches["match_date"] = pd.to_datetime(matches["match_date"])
        matches = matches[matches["match_date"].between(str(start_date), str(end_date))]

    if matches.empty:
        st.warning("No available matches found for the selected filters.")
    else:
        list_matches = matches["match_id"].tolist()
        st.info(f"Found {len(list_matches)} match(es). Fetching events...")

        events = []
        progress = st.progress(0)
        for i, match_id in enumerate(list_matches):
            events.append(sb.events(match_id=match_id, creds=CREDS))
            progress.progress((i + 1) / len(list_matches))

        events_df = pd.concat(events, ignore_index=True)
        st.success(f"Fetched {len(events_df):,} events from {len(list_matches)} match(es).")
        st.dataframe(events_df.head(200), use_container_width=True)

        csv = events_df.to_csv(index=False)
        st.download_button(
            label="Download CSV",
            data=csv,
            file_name=f"statsbomb_{team.replace(' ', '_')}_{start_date}_{end_date}.csv",
            mime="text/csv",
        )
