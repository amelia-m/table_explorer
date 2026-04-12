"""
Table Relationship Explorer — Streamlit App
Supports CSV uploads and JSON/YAML schema definitions.
"""

import streamlit as st
import pandas as pd
import networkx as nx
import json
import yaml
import re
import io
import hashlib
from pathlib import Path
from typing import Optional

# ─── Page config ────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="Table Relationship Explorer",
    page_icon="◫",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Force sidebar open — clears any persisted collapsed state in browser localStorage
import streamlit.components.v1 as _components
_components.html("""
<script>
(function() {
  // Clear Streamlit's persisted sidebar state so it always starts expanded
  try {
    var keys = Object.keys(localStorage);
    keys.forEach(function(k) {
      if (k.indexOf('sidebar') !== -1 || k.indexOf('Sidebar') !== -1) {
        localStorage.removeItem(k);
      }
    });
  } catch(e) {}
})();
</script>
""", height=0)

# ─── Inject CSS ──────────────────────────────────────────────────────────────
# ── CSS: theme variables are generated from Python session state ─────────
# This is the key insight: Streamlit re-runs this on every interaction,
# so we just pick the right :root values directly — no JS needed at all.
_DARK = dict(
    bg="#141e30", surface="#1a2640", card="#1e2d4a",
    border="#2a3f60", border2="#344f78",
    accent="#5badff", green="#4ade80", orange="#fb923c",
    purple="#c084fc", yellow="#fbbf24", red="#f87171", teal="#34d399",
    text="#e8eef6", text2="#9ab0cc", text3="#5a7898",
    sig_naming="#4ade80", sig_namesim="#34d399", sig_overlap="#fb923c",
    sig_card="#fbbf24", sig_fmt="#c084fc", sig_dist="#5badff",
    sig_null="#f97316", sig_manual="#f87171", sig_schema="#5badff",
)
_LIGHT = dict(
    bg="#f0f4fa", surface="#ffffff", card="#f7f9fc",
    border="#d0daea", border2="#a8bdd4",
    accent="#1a62c7", green="#166534", orange="#b84208",
    purple="#5b21b6", yellow="#854d0e", red="#991b1b", teal="#0d6e66",
    text="#0d1829", text2="#334155", text3="#64748b",
    sig_naming="#166534", sig_namesim="#0d6e66", sig_overlap="#b84208",
    sig_card="#854d0e", sig_fmt="#5b21b6", sig_dist="#1a62c7",
    sig_null="#7c2d12", sig_manual="#991b1b", sig_schema="#1a62c7",
)
_T = _DARK if st.session_state.get("dark_mode", True) else _LIGHT

st.markdown(f"""<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500;700&display=swap');

/* ── Theme variables (generated from Python) ── */
:root {{
  --bg:        {_T["bg"]};
  --surface:   {_T["surface"]};
  --card:      {_T["card"]};
  --border:    {_T["border"]};
  --border2:   {_T["border2"]};
  --accent:    {_T["accent"]};
  --green:     {_T["green"]};
  --orange:    {_T["orange"]};
  --purple:    {_T["purple"]};
  --yellow:    {_T["yellow"]};
  --red:       {_T["red"]};
  --teal:      {_T["teal"]};
  --text:      {_T["text"]};
  --text2:     {_T["text2"]};
  --text3:     {_T["text3"]};
  --mono:      'JetBrains Mono', 'Fira Code', monospace;
  --sans:      'Inter', system-ui, sans-serif;
  --sig-naming:   {_T["sig_naming"]};
  --sig-namesim:  {_T["sig_namesim"]};
  --sig-overlap:  {_T["sig_overlap"]};
  --sig-card:     {_T["sig_card"]};
  --sig-fmt:      {_T["sig_fmt"]};
  --sig-dist:     {_T["sig_dist"]};
  --sig-null:     {_T["sig_null"]};
  --sig-manual:   {_T["sig_manual"]};
  --sig-schema:   {_T["sig_schema"]};
}}

html, body, [class*="css"] {{
  font-family: var(--sans) !important;
  font-feature-settings: 'cv02', 'cv03', 'cv04', 'tnum' !important;
  -webkit-font-smoothing: antialiased !important;
  background: var(--bg) !important;
  color: var(--text) !important;
}}

/* ── Background: all Streamlit containers use theme bg ── */
.stApp, [data-testid="stAppViewContainer"],
[data-testid="stMain"], [data-testid="stMainBlockContainer"],
.block-container {{
  background: var(--bg) !important;
}}

/* ── Streamlit chrome ── */
#MainMenu, footer {{ visibility: hidden; }}
header {{ background: var(--surface) !important; border-bottom: 1px solid var(--border) !important; }}

/* ── Layout ── */
.block-container {{ padding: 1rem 2rem !important; max-width: 100% !important; }}
section[data-testid="stSidebar"] {{
  background: var(--surface) !important;
  border-right: 1px solid var(--border) !important;
}}
/* Remove Streamlit's default top padding inside sidebar */
section[data-testid="stSidebar"] > div:first-child {{
  padding-top: 0.5rem !important;
}}
/* Make the sidebar collapse arrow bigger and more visible */
[data-testid="stSidebarCollapseButton"] button,
[data-testid="stSidebarCollapsedControl"] button {{
  color: var(--text2) !important;
  background: var(--card) !important;
  border: 1px solid var(--border2) !important;
  border-radius: 6px !important;
  width: 28px !important; height: 28px !important;
  opacity: 1 !important;
}}
[data-testid="stSidebarCollapseButton"] button:hover,
[data-testid="stSidebarCollapsedControl"] button:hover {{
  color: var(--accent) !important;
  border-color: var(--accent) !important;
  background: var(--surface) !important;
}}
[data-testid="stSidebarCollapseButton"] svg,
[data-testid="stSidebarCollapsedControl"] svg {{
  width: 16px !important; height: 16px !important;
  fill: currentColor !important;
}}

/* ── Theme toggle button ────────────────────────── */
#theme-toggle-btn {{
  display: flex; align-items: center; gap: 8px;
  background: var(--card); border: 1px solid var(--border);
  border-radius: 20px; padding: 6px 14px;
  cursor: pointer; font-family: var(--mono);
  font-size: 11px; color: var(--text2);
  white-space: nowrap; transition: all 0.2s;
  flex-shrink: 0;
}}
#theme-toggle-btn:hover {{ border-color: var(--accent); color: var(--accent); }}
.toggle-track {{
  width: 30px; height: 16px; background: var(--border2);
  border-radius: 8px; position: relative; transition: background 0.2s; flex-shrink: 0;
}}
.toggle-thumb {{
  width: 11px; height: 11px; background: #fff; border-radius: 50%;
  position: absolute; top: 2.5px; left: 2.5px; transition: left 0.2s;
  box-shadow: 0 1px 3px rgba(0,0,0,0.3);
}}

/* ── Sidebar labels ─────────────────────────────── */
.sidebar-section {{
  font-family: var(--mono); font-size: 10px; font-weight: 700;
  letter-spacing: 2.5px; text-transform: uppercase;
  color: var(--accent); margin: 20px 0 10px;
  padding-bottom: 6px; border-bottom: 1px solid var(--border);
}}
.sidebar-hint {{ font-size: 12px; color: var(--text3); margin: -4px 0 10px; line-height: 1.6; font-family: var(--sans); }}

/* ── Pills ──────────────────────────────────────── */
.pill {{
  display: inline-block; padding: 2px 9px; border-radius: 20px;
  font-size: 11px; font-family: var(--mono); margin: 2px;
  border: 1px solid transparent;
}}
.p-rows  {{ background: rgba(61,214,140,0.10); color: var(--green);  border-color: rgba(61,214,140,0.25); }}
.p-cols  {{ background: rgba(77,166,255,0.10); color: var(--accent); border-color: rgba(77,166,255,0.25); }}
.p-pk    {{ background: rgba(245,200,66,0.12); color: var(--yellow); border-color: rgba(245,200,66,0.30); }}
.p-fk    {{ background: rgba(167,139,250,0.12);color: var(--purple); border-color: rgba(167,139,250,0.28); }}
.p-warn  {{ background: rgba(242,107,122,0.12);color: var(--red);    border-color: rgba(242,107,122,0.28); }}
.p-schema{{ background: rgba(245,146,78,0.10); color: var(--orange); border-color: rgba(245,146,78,0.25); }}

/* ── Table cards ────────────────────────────────── */
.tbl-card {{
  background: var(--card); border: 1px solid var(--border);
  border-radius: 10px; padding: 18px; margin-bottom: 16px;
  transition: border-color 0.2s;
}}
.tbl-card:hover {{ border-color: var(--border2); }}
.tbl-card-title {{
  font-family: var(--mono); font-size: 14px; color: var(--accent);
  margin: 0 0 12px; display: flex; align-items: center; gap: 8px;
}}
.tbl-card-title .src-badge {{
  font-size: 9px; padding: 2px 7px; border-radius: 10px;
  background: rgba(245,146,78,0.10); color: var(--orange);
  border: 1px solid rgba(245,146,78,0.22);
}}

/* ── Relationship rows ──────────────────────────── */
.rel-row {{
  display: flex; align-items: center; gap: 8px;
  padding: 9px 14px; margin: 4px 0;
  background: var(--card); border: 1px solid var(--border);
  border-radius: 8px; font-size: 13px;
  font-family: var(--sans);
  transition: border-color 0.15s;
}}
.rel-row:hover {{ border-color: var(--border2); }}
.rt {{ font-family: var(--mono); font-weight: 700; color: var(--accent); font-size: 13px; }}
.rc {{ font-family: var(--mono); color: var(--text2); font-size: 12px; }}
.ra {{ color: var(--border2); font-size: 14px; }}
.rm {{
  font-size: 10px; padding: 2px 9px; border-radius: 10px;
  font-family: var(--mono); margin-left: auto;
  border: 1px solid transparent;
}}
.m-naming        {{ background: rgba(61,214,140,0.12); color: var(--sig-naming);  border-color: rgba(61,214,140,0.25); }}
.m-name_similarity{{ background: rgba(45,212,191,0.12); color: var(--sig-namesim); border-color: rgba(45,212,191,0.25); }}
.m-value_overlap  {{ background: rgba(245,146,78,0.12); color: var(--sig-overlap); border-color: rgba(245,146,78,0.25); }}
.m-cardinality    {{ background: rgba(245,200,66,0.12); color: var(--sig-card);    border-color: rgba(245,200,66,0.25); }}
.m-format         {{ background: rgba(167,139,250,0.12);color: var(--sig-fmt);     border-color: rgba(167,139,250,0.25); }}
.m-distribution   {{ background: rgba(77,166,255,0.12); color: var(--sig-dist);   border-color: rgba(77,166,255,0.25); }}
.m-null_pattern   {{ background: rgba(251,146,60,0.12); color: var(--sig-null);   border-color: rgba(251,146,60,0.25); }}
.m-manual         {{ background: rgba(242,107,122,0.12);color: var(--sig-manual); border-color: rgba(242,107,122,0.25); }}
.m-schema         {{ background: rgba(77,166,255,0.12); color: var(--sig-schema); border-color: rgba(77,166,255,0.25); }}
.m-content        {{ background: rgba(167,139,250,0.12);color: var(--sig-fmt);    border-color: rgba(167,139,250,0.25); }}

/* Confidence bar */
.conf-bar {{ display:flex; align-items:center; gap:6px; margin-left:auto; }}
.conf-dot {{ width:8px; height:8px; border-radius:50%; flex-shrink:0; }}
.conf-high   .conf-dot {{ background: var(--green); }}
.conf-medium .conf-dot {{ background: var(--orange); }}
.conf-low    .conf-dot {{ background: var(--text3); }}
.conf-label {{ font-size:11px; font-family:var(--sans); font-weight:500; }}
.conf-high   .conf-label {{ color: var(--green); }}
.conf-medium .conf-label {{ color: var(--orange); }}
.conf-low    .conf-label {{ color: var(--text3); }}
.conf-score {{ font-size:10px; color: var(--text3); font-family:var(--sans); }}

/* Signal chips */
.signal-chips {{ display:flex; flex-wrap:wrap; gap:3px; margin-top:4px; padding-left:4px; }}
.sig-chip {{
  font-size:9px; font-family:var(--mono); padding:1px 7px; border-radius:8px;
  background: var(--card); border: 1px solid var(--border); color: var(--text3);
}}

/* ── Section headers ────────────────────────────── */
.rel-section {{
  font-family: var(--sans); font-size: 11px; font-weight: 600; letter-spacing: 1.5px;
  text-transform: uppercase; color: var(--text3);
  padding: 14px 0 6px;
}}

/* ── ERD hint ───────────────────────────────────── */
.erd-hint {{
  font-size: 11px; color: var(--text3); text-align: center;
  padding: 8px; font-family: var(--sans); letter-spacing: 0.2px;
}}

/* ── Legend ─────────────────────────────────────── */
.legend {{ display: flex; gap: 20px; padding: 10px 0 14px; flex-wrap: wrap; }}
.li {{ display: flex; align-items: center; gap: 6px; font-size: 11px; color: var(--text2); font-family: var(--mono); }}
.ld {{ width: 10px; height: 10px; border-radius: 50%; }}

/* ── Loaded tables list ─────────────────────────── */
.tbl-item {{
  display: flex; align-items: center; justify-content: space-between;
  padding: 6px 10px; margin: 3px 0;
  background: var(--bg); border: 1px solid var(--border);
  border-radius: 6px;
}}
.tbl-item .tn {{ font-family: var(--mono); font-size: 11px; color: var(--text); }}
.tbl-item .tm {{ font-size: 10px; color: var(--text3); font-family: var(--mono); }}

/* ── Empty state ────────────────────────────────── */
.empty-state {{
  text-align: center; padding: 80px 40px;
  color: var(--text3); font-family: var(--sans);
}}
.empty-state .icon {{ font-size: 48px; margin-bottom: 16px; }}
.empty-state h3 {{ color: var(--text2); font-size: 18px; font-weight: 600; margin-bottom: 8px; font-family: var(--sans); }}

/* ── Streamlit overrides ────────────────────────── */
.stTabs [data-baseweb="tab-list"] {{
  background: transparent !important;
  border-bottom: 1px solid var(--border) !important;
  gap: 4px;
}}
.stTabs [data-baseweb="tab"] {{
  font-family: var(--mono) !important; font-size: 11px !important;
  color: var(--text3) !important; letter-spacing: 1px !important;
  text-transform: uppercase !important;
  background: transparent !important;
  border: none !important; padding: 10px 16px !important;
}}
.stTabs [aria-selected="true"] {{
  color: var(--accent) !important;
  border-bottom: 2px solid var(--accent) !important;
  background: transparent !important;
}}
.stTabs [data-baseweb="tab-panel"] {{
  background: var(--surface) !important;
  border: 1px solid var(--border) !important;
  border-top: none !important; border-radius: 0 0 10px 10px !important;
  padding: 20px !important;
}}
.stSelectbox > div > div {{
  background: var(--card) !important;
  border: 1px solid var(--border2) !important;
  color: var(--text) !important;
  border-radius: 6px !important;
}}
/* ── Comprehensive Streamlit widget overrides ── */

/* File uploader */
.stFileUploader {{
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  border-radius: 10px !important;
}}
.stFileUploader label, 
.stFileUploader span,
.stFileUploader p,
.stFileUploader small,
[data-testid="stFileUploaderDropzone"] span,
[data-testid="stFileUploaderDropzone"] p,
[data-testid="stFileUploaderDropzone"] small {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
}}
[data-testid="stFileUploaderDropzone"] {{
  background: var(--surface) !important;
  border: 2px dashed var(--border2) !important;
  border-radius: 8px !important;
}}
[data-testid="stFileUploaderDropzone"] button,
[data-testid="stFileUploaderDropzone"] [kind="secondary"] {{
  font-family: var(--sans) !important;
  background: var(--bg) !important;
  color: var(--text) !important;
  border: 1px solid var(--border2) !important;
  border-radius: 6px !important;
}}

/* All buttons */
.stButton > button,
button[kind="secondary"],
button[kind="primary"] {{
  font-family: var(--sans) !important;
  font-weight: 500 !important;
  font-size: 13px !important;
  letter-spacing: 0.2px !important;
  border-radius: 7px !important;
  background: var(--card) !important;
  border: 1px solid var(--border2) !important;
  color: var(--text) !important;
  transition: all 0.15s !important;
  padding: 6px 14px !important;
}}
.stButton > button:hover {{
  border-color: var(--accent) !important;
  color: var(--accent) !important;
  background: var(--surface) !important;
}}
button[kind="primary"] {{
  background: var(--accent) !important;
  color: var(--bg) !important;
  border-color: var(--accent) !important;
}}

/* Labels / text everywhere */
label, .stSelectbox label, .stCheckbox label,
[data-testid="stWidgetLabel"] span,
[data-testid="stWidgetLabel"] p {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
  font-size: 13px !important;
}}

/* Selectbox */
.stSelectbox > div > div {{
  background: var(--card) !important;
  border: 1px solid var(--border2) !important;
  color: var(--text) !important;
  border-radius: 6px !important;
  font-family: var(--sans) !important;
}}

/* Checkboxes */
.stCheckbox span {{
  font-family: var(--sans) !important;
  color: var(--text) !important;
}}

/* Expanders */
[data-testid="stExpander"] {{
  background: var(--card) !important;
  border: 1px solid var(--border) !important;
  border-radius: 8px !important;
}}
[data-testid="stExpander"] summary span {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
}}

/* Select slider */
.stSlider label, .stSlider span {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
}}

/* Success / info / warning banners */
.stAlert {{
  background: var(--card) !important;
  border-color: var(--border2) !important;
  color: var(--text) !important;
  font-family: var(--sans) !important;
}}

/* Dataframe */
div[data-testid="stDataFrame"] {{
  background: var(--card) !important;
  border-radius: 8px !important;
}}

/* Spinner text */
.stSpinner p {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
}}

/* File list items in uploader */
[data-testid="stFileUploaderFile"] {{
  background: var(--surface) !important;
  border: 1px solid var(--border) !important;
  border-radius: 6px !important;
}}
[data-testid="stFileUploaderFile"] span,
[data-testid="stFileUploaderFile"] p {{
  font-family: var(--sans) !important;
  color: var(--text2) !important;
}}

/* Page navigation */
[data-testid="stFileUploaderFileListSection"] small {{
  font-family: var(--sans) !important;
  color: var(--text3) !important;
}}

.stMarkdown h3 {{ font-family: var(--mono) !important; color: var(--accent) !important; font-size: 14px !important; }}

</style>""", unsafe_allow_html=True)


# ═══════════════════════════════════════════════════════════════════════════
# Helper functions
# ═══════════════════════════════════════════════════════════════════════════

def clean_name(name: str) -> str:
    """Normalize a column/table name: lowercase, non-alnum → underscore, collapse."""
    n = re.sub(r"[^a-z0-9]", "_", name.lower())
    n = re.sub(r"_+", "_", n).strip("_")
    return n


def id_stem(col_clean: str) -> str:
    """'document_id' → 'document'"""
    return re.sub(r"_id$", "", col_clean)


def is_pk_name(col_clean: str, tname_clean: str) -> bool:
    if col_clean == "id":
        return True
    if col_clean == f"{tname_clean}_id":
        return True
    if col_clean.endswith("_id") and tname_clean.startswith(id_stem(col_clean)):
        return True
    return False


def is_fk_for(col_clean: str, t2_clean: str) -> bool:
    if col_clean == f"{t2_clean}_id":
        return True
    if col_clean.endswith("_id") and t2_clean.startswith(id_stem(col_clean)):
        return True
    return False


def detect_pks(df: pd.DataFrame, table_name: str, method: str = "both") -> list[str]:
    """
    Return at most ONE primary key column, chosen by priority:
      1. Naming match (id / {table}_id)   — highest confidence
      2. First all-unique, non-null column whose name ends in _id / _key
      3. First all-unique, non-null column (any name)
    Returning a single PK avoids confusing unique-but-not-PK columns
    (e.g. email, name) being flagged as primary keys.
    """
    cols = df.columns.tolist()
    n = len(df)

    # Priority 1: naming convention match
    if method in ("naming", "both"):
        tname = clean_name(table_name)
        for col in cols:
            if is_pk_name(clean_name(col), tname):
                return [col]

    # Priority 2 & 3: uniqueness fallback (only when naming gave nothing)
    if method in ("uniqueness", "both", "content", "all") and n > 0:
        unique_cols = [
            c for c in cols
            if df[c].notna().all() and df[c].nunique() == n
        ]
        # Prefer columns whose name looks like an ID
        id_like = [c for c in unique_cols
                   if re.search(r"(_id|_key|_no|_num|_code)$", clean_name(c))]
        if id_like:
            return [id_like[0]]
        if unique_cols:
            return [unique_cols[0]]

    return []


def detect_composite_pks(
    df: pd.DataFrame,
    table_name: str,
    max_combo_size: int = 3,
    max_candidates: int = 20,
) -> list[list[str]]:
    """
    Detect composite (multi-column) primary keys.

    A composite key is a minimal set of columns whose combined values
    uniquely identify every row, where no single column alone is unique.

    Returns a list of column-name lists, one per detected composite key.
    Returns an empty list if a single-column PK already exists or none found.
    """
    n = len(df)
    if n < 2:
        return []

    # Skip if a single-column PK already exists
    if detect_pks(df, table_name, method="both"):
        return []

    cols = df.columns.tolist()

    # Collect candidate columns: skip booleans, all-NA, and long free-text
    def _is_candidate(col: str) -> bool:
        s = df[col]
        if s.dtype == bool:
            return False
        clean = s.dropna()
        if len(clean) == 0:
            return False
        if s.dtype == object:
            sample = clean.head(50).astype(str)
            if sample.str.len().median() > 80:
                return False
        return True

    candidates = [c for c in cols if _is_candidate(c)]

    if len(candidates) < 2:
        return []

    # Sort: id-like columns first for faster discovery
    id_re = re.compile(r"(_id|_key|id$|key$|_code|_num|_no)$")
    id_like = [c for c in candidates if id_re.search(clean_name(c))]
    non_id  = [c for c in candidates if c not in id_like]
    candidates = (id_like + non_id)[:max_candidates]

    # Try 2-column combinations
    for i, c1 in enumerate(candidates[:-1]):
        for c2 in candidates[i + 1:]:
            if df[c1].isna().any() or df[c2].isna().any():
                continue
            combined = df[c1].astype(str) + "\x01" + df[c2].astype(str)
            if combined.nunique() == n:
                return [[c1, c2]]

    # Try 3-column combinations
    if max_combo_size >= 3 and len(candidates) >= 3:
        for i, c1 in enumerate(candidates[:-2]):
            for j, c2 in enumerate(candidates[i + 1:], start=i + 1):
                for c3 in candidates[j + 1:]:
                    if df[c1].isna().any() or df[c2].isna().any() or df[c3].isna().any():
                        continue
                    combined = (
                        df[c1].astype(str) + "\x01"
                        + df[c2].astype(str) + "\x01"
                        + df[c3].astype(str)
                    )
                    if combined.nunique() == n:
                        return [[c1, c2, c3]]

    return []


# ═══════════════════════════════════════════════════════════════════════════
# Content-based inference helpers
# ═══════════════════════════════════════════════════════════════════════════

# Confidence thresholds
OVERLAP_HIGH   = 0.98   # ≥98% of FK values found in PK column → high-confidence
OVERLAP_MEDIUM = 0.80   # ≥80% → medium (partial match / sampled data)
NAME_SIM_HIGH  = 0.85   # Jaro-Winkler ≥ 0.85 → strong name similarity
NAME_SIM_MED   = 0.72   # Jaro-Winkler ≥ 0.72 → moderate name similarity
DIST_SIM_HIGH  = 0.90   # Distribution cosine similarity ≥ 0.90
DIST_SIM_MED   = 0.75

# Value format fingerprint patterns
FORMAT_PATTERNS = [
    ("uuid",     re.compile(r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", re.I)),
    ("email",    re.compile(r"^[^@\s]+@[^@\s]+\.[^@\s]+$")),
    ("zip_us",   re.compile(r"^\d{5}(-\d{4})?$")),
    ("phone",    re.compile(r"^\+?[\d\s\-().]{7,15}$")),
    ("iso_date", re.compile(r"^\d{4}-\d{2}-\d{2}$")),
    ("iso_ts",   re.compile(r"^\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}")),
    ("hex_color",re.compile(r"^#[0-9a-f]{3,6}$", re.I)),
    ("int_code", re.compile(r"^\d{1,6}$")),
    ("alpha_code",re.compile(r"^[A-Z]{2,4}$")),
]

def _jaro_winkler(s1: str, s2: str) -> float:
    """Pure-Python Jaro-Winkler similarity (no external dep)."""
    if s1 == s2:
        return 1.0
    l1, l2 = len(s1), len(s2)
    if l1 == 0 or l2 == 0:
        return 0.0
    match_dist = max(l1, l2) // 2 - 1
    if match_dist < 0:
        match_dist = 0
    s1_matches = [False] * l1
    s2_matches = [False] * l2
    matches = 0
    transpositions = 0
    for i in range(l1):
        lo = max(0, i - match_dist)
        hi = min(i + match_dist + 1, l2)
        for j in range(lo, hi):
            if s2_matches[j] or s1[i] != s2[j]:
                continue
            s1_matches[i] = True
            s2_matches[j] = True
            matches += 1
            break
    if matches == 0:
        return 0.0
    k = 0
    for i in range(l1):
        if not s1_matches[i]:
            continue
        while not s2_matches[k]:
            k += 1
        if s1[i] != s2[k]:
            transpositions += 1
        k += 1
    jaro = (matches/l1 + matches/l2 + (matches - transpositions/2)/matches) / 3
    # Winkler prefix bonus
    prefix = 0
    for i in range(min(4, l1, l2)):
        if s1[i] == s2[i]:
            prefix += 1
        else:
            break
    return jaro + prefix * 0.1 * (1 - jaro)


def _col_dtype_class(series: pd.Series) -> str:
    """Coarse type bucket: numeric | datetime | string."""
    if pd.api.types.is_numeric_dtype(series):
        return "numeric"
    if pd.api.types.is_datetime64_any_dtype(series):
        return "datetime"
    return "string"


def _format_fingerprint(series: pd.Series, sample: int = 200) -> str | None:
    """
    Sample up to `sample` non-null values and test what format ≥80% match.
    Returns the format name or None.
    """
    vals = series.dropna().astype(str)
    if len(vals) == 0:
        return None
    probe = vals.sample(min(sample, len(vals)), random_state=42)
    for name, pat in FORMAT_PATTERNS:
        hits = probe.str.match(pat).sum()
        if hits / len(probe) >= 0.80:
            return name
    return None


def _value_overlap(vals1: pd.Series, vals2: pd.Series) -> float:
    """
    Fraction of non-null values in vals1 that appear in vals2.
    Fast set intersection; capped at 50k unique values each for performance.
    """
    s1 = set(vals1.dropna().astype(str).unique()[:50_000])
    s2 = set(vals2.dropna().astype(str).unique()[:50_000])
    if not s1:
        return 0.0
    return len(s1 & s2) / len(s1)


def _distribution_similarity(vals1: pd.Series, vals2: pd.Series) -> float:
    """
    Cosine similarity between value-frequency histograms of two columns.
    Works for both numeric and categorical data.
    Scores near 1.0 mean the columns share similar value distributions.
    """
    import numpy as np
    from scipy.sparse import csr_matrix

    # Build unified vocabulary
    s1 = vals1.dropna().astype(str).value_counts()
    s2 = vals2.dropna().astype(str).value_counts()
    if s1.empty or s2.empty:
        return 0.0

    vocab = list(set(s1.index) | set(s2.index))
    v1 = np.array([s1.get(w, 0) for w in vocab], dtype=float)
    v2 = np.array([s2.get(w, 0) for w in vocab], dtype=float)

    norm1 = np.linalg.norm(v1)
    norm2 = np.linalg.norm(v2)
    if norm1 == 0 or norm2 == 0:
        return 0.0
    return float(np.dot(v1, v2) / (norm1 * norm2))


def _null_pattern_correlation(df1: pd.DataFrame, col1: str,
                               df2: pd.DataFrame, col2: str) -> float:
    """
    Pearson correlation between null masks of two columns that share the
    same number of rows. Returns 0.0 if inapplicable.
    """
    if len(df1) != len(df2):
        return 0.0
    try:
        from scipy.stats import pearsonr
        mask1 = df1[col1].isna().astype(int)
        mask2 = df2[col2].isna().astype(int)
        if mask1.std() == 0 or mask2.std() == 0:
            return 0.0
        r, _ = pearsonr(mask1, mask2)
        return float(r)
    except Exception:
        return 0.0


def _score_candidate(
    t1: str, col1: str, df1: pd.DataFrame,
    t2: str, col2: str, df2: pd.DataFrame,
    enable_flags: dict,
) -> dict | None:
    """
    Run all enabled analyses on a (t1.col1 → t2.col2) candidate pair.
    Returns a result dict with signals and composite confidence, or None
    if no signal passes the minimum bar.
    """
    import numpy as np

    signals = {}
    reasons = []

    # ── 1. Naming conventions ────────────────────────────────────────────
    if enable_flags.get("naming"):
        c1 = clean_name(col1)
        t2c = clean_name(t2)
        if is_fk_for(c1, t2c):
            signals["naming_exact"] = 1.0
            reasons.append("exact FK naming")
        else:
            # Fuzzy column name similarity (strip _id suffix before comparing)
            stem1 = re.sub(r"_(id|key|code|num|no)$", "", c1)
            stem2 = re.sub(r"_(id|key|code|num|no)$", "", clean_name(col2))
            sim = _jaro_winkler(stem1, stem2)
            if sim >= NAME_SIM_HIGH:
                signals["name_sim"] = sim
                reasons.append(f"name similarity {sim:.2f}")
            elif sim >= NAME_SIM_MED:
                signals["name_sim_weak"] = sim
                reasons.append(f"weak name similarity {sim:.2f}")

    # ── 2. Type compatibility guard ──────────────────────────────────────
    dtype1 = _col_dtype_class(df1[col1])
    dtype2 = _col_dtype_class(df2[col2])
    if dtype1 != dtype2:
        return None  # incompatible types → not a candidate

    # ── 3. Value overlap (unconstrained — no _id name required) ─────────
    if enable_flags.get("value_overlap") and len(df1) > 0 and len(df2) > 0:
        overlap = _value_overlap(df1[col1], df2[col2])
        if overlap >= OVERLAP_HIGH:
            signals["overlap_high"] = overlap
            reasons.append(f"value overlap {overlap:.0%}")
        elif overlap >= OVERLAP_MEDIUM:
            signals["overlap_medium"] = overlap
            reasons.append(f"partial overlap {overlap:.0%}")

    # ── 4. Exact cardinality match ───────────────────────────────────────
    if enable_flags.get("cardinality") and len(df1) > 0 and len(df2) > 0:
        u1 = set(df1[col1].dropna().astype(str))
        u2 = set(df2[col2].dropna().astype(str))
        if u1 and u2 and u1 == u2:
            signals["cardinality_match"] = 1.0
            reasons.append("identical value sets")

    # ── 5. Format fingerprint ────────────────────────────────────────────
    if enable_flags.get("format") and len(df1) > 0 and len(df2) > 0:
        fmt1 = _format_fingerprint(df1[col1])
        fmt2 = _format_fingerprint(df2[col2])
        if fmt1 and fmt2 and fmt1 == fmt2:
            signals["format_match"] = 0.6
            reasons.append(f"shared format [{fmt1}]")

    # ── 6. Distribution similarity ───────────────────────────────────────
    if enable_flags.get("distribution") and len(df1) > 0 and len(df2) > 0:
        dist_sim = _distribution_similarity(df1[col1], df2[col2])
        if dist_sim >= DIST_SIM_HIGH:
            signals["dist_high"] = dist_sim
            reasons.append(f"distribution similarity {dist_sim:.2f}")
        elif dist_sim >= DIST_SIM_MED:
            signals["dist_med"] = dist_sim
            reasons.append(f"weak distribution similarity {dist_sim:.2f}")

    # ── 7. Null pattern correlation ──────────────────────────────────────
    if enable_flags.get("null_pattern") and len(df1) == len(df2):
        null_r = _null_pattern_correlation(df1, col1, df2, col2)
        if null_r >= 0.80:
            signals["null_corr"] = null_r
            reasons.append(f"null pattern corr {null_r:.2f}")

    if not signals:
        return None

    # ── Composite confidence score ───────────────────────────────────────
    # Each signal class contributes a weighted vote; cap at 1.0
    weight_map = {
        "naming_exact":      1.00,
        "cardinality_match": 0.95,
        "overlap_high":      0.90,
        "name_sim":          0.60,
        "overlap_medium":    0.55,
        "dist_high":         0.50,
        "format_match":      0.40,
        "dist_med":          0.30,
        "name_sim_weak":     0.25,
        "null_corr":         0.20,
    }
    # Use a "noisy OR" combination: 1 - Π(1 - wᵢ)
    import math
    score = 1.0 - math.prod(1.0 - weight_map.get(k, 0.1) for k in signals)
    score = round(min(score, 1.0), 3)

    # Determine primary detected_by label (highest-weight signal)
    top_signal = max(signals, key=lambda k: weight_map.get(k, 0.1))
    label_map = {
        "naming_exact":      "naming",
        "name_sim":          "name_similarity",
        "name_sim_weak":     "name_similarity",
        "overlap_high":      "value_overlap",
        "overlap_medium":    "value_overlap",
        "cardinality_match": "cardinality",
        "format_match":      "format",
        "dist_high":         "distribution",
        "dist_med":          "distribution",
        "null_corr":         "null_pattern",
    }
    detected_by = label_map.get(top_signal, "content")

    # Confidence tier
    if score >= 0.85:
        confidence = "high"
    elif score >= 0.55:
        confidence = "medium"
    else:
        confidence = "low"

    return {
        "signals":     signals,
        "reasons":     reasons,
        "score":       score,
        "confidence":  confidence,
        "detected_by": detected_by,
    }


def detect_fks(
    tables: dict[str, pd.DataFrame],
    method: str = "both",
    min_confidence: str = "medium",
    enable_flags: dict | None = None,
) -> list[dict]:
    """
    Multi-signal FK detection.
    method controls which signal families are active:
      "naming"     → naming signals only
      "content"    → all content-based signals (no naming)
      "both"/"all" → everything
      "manual"     → skip auto-detection entirely
    min_confidence: "low" | "medium" | "high" — minimum tier to emit
    enable_flags: fine-grained per-signal overrides
    """
    if method == "manual" or len(tables) < 2:
        return []

    conf_rank = {"low": 0, "medium": 1, "high": 2}
    min_rank = conf_rank.get(min_confidence, 1)

    # Build enable_flags from method
    if enable_flags is None:
        use_naming  = method in ("naming", "both", "all")
        use_content = method in ("content", "both", "all", "uniqueness")
        enable_flags = {
            "naming":       use_naming,
            "value_overlap": use_content,
            "cardinality":  use_content,
            "format":       use_content,
            "distribution": use_content,
            "null_pattern": use_content,
        }

    tnames = list(tables.keys())

    # Pre-compute PK columns per table (all-unique + no-nulls)
    pk_map = {}
    for t in tnames:
        df = tables[t]
        n = len(df)
        pk_map[t] = [c for c in df.columns
                     if n > 0 and df[c].notna().all() and df[c].nunique() == n]

    results: list[dict] = []
    seen: set[str] = set()     # "t1|col1|t2" — one rel per (table, col, target)
    best: dict[str, float] = {}  # "t1|col1" → best score so far (pick best target)

    for t1 in tnames:
        df1 = tables[t1]
        if len(df1) == 0 and not enable_flags.get("naming"):
            continue

        for col1 in df1.columns:
            if col1 in pk_map[t1]:
                continue  # skip PKs of own table

            too_large = len(df1) > 150_000 or len(df1.columns) > 300

            for t2 in tnames:
                if t2 == t1:
                    continue

                rel_key = f"{t1}|{col1}|{t2}"
                if rel_key in seen:
                    continue

                df2 = tables[t2]

                # For content signals we compare col1 against every candidate
                # PK column in t2 (and also all-unique cols); pick best match
                target_cols = pk_map[t2] if pk_map[t2] else list(df2.columns)
                # Limit scan for large tables
                if too_large:
                    target_cols = [c for c in target_cols
                                   if bool(re.search(r"(_id|_key|id$|key$)",
                                                     clean_name(c)))]

                best_result = None
                best_to_col = None

                for col2 in target_cols:
                    result = _score_candidate(
                        t1, col1, df1,
                        t2, col2, df2,
                        enable_flags,
                    )
                    if result is None:
                        continue
                    if best_result is None or result["score"] > best_result["score"]:
                        best_result = result
                        best_to_col = col2

                if best_result is None:
                    continue
                if conf_rank.get(best_result["confidence"], 0) < min_rank:
                    continue

                # Deduplicate: if col1 already has a higher-scoring rel, skip
                col_key = f"{t1}|{col1}"
                if col_key in best and best[col_key] > best_result["score"] + 0.05:
                    continue
                best[col_key] = best_result["score"]

                seen.add(rel_key)
                results.append(dict(
                    from_table  = t1,
                    from_col    = col1,
                    to_table    = t2,
                    to_col      = best_to_col,
                    detected_by = best_result["detected_by"],
                    confidence  = best_result["confidence"],
                    score       = best_result["score"],
                    reasons     = best_result["reasons"],
                    signals     = best_result["signals"],
                ))

    # Sort by confidence desc, score desc
    results.sort(key=lambda r: (-conf_rank.get(r["confidence"], 0), -r["score"]))
    return results


def parse_schema_json(raw: str) -> tuple[dict[str, pd.DataFrame], list[dict]]:
    """
    Parse a JSON schema definition. Expected format:
    {
      "tables": [
        {
          "name": "orders",
          "columns": [
            {"name": "order_id", "type": "integer", "primary_key": true},
            {"name": "customer_id", "type": "integer", "foreign_key": {"table": "customers", "column": "customer_id"}}
          ]
        }
      ]
    }
    Returns (tables_as_empty_dfs, explicit_rels)
    """
    data = json.loads(raw)
    tables = {}
    rels = []

    for tbl in data.get("tables", []):
        tname = tbl["name"]
        cols = [c["name"] for c in tbl.get("columns", [])]
        tables[tname] = pd.DataFrame(columns=cols)
        tables[tname].attrs["source"] = "schema"
        tables[tname].attrs["columns_meta"] = tbl.get("columns", [])

        for col in tbl.get("columns", []):
            fk = col.get("foreign_key")
            if fk:
                rels.append(dict(
                    from_table=tname,
                    from_col=col["name"],
                    to_table=fk["table"],
                    to_col=fk.get("column"),
                    detected_by="schema",
                ))

    return tables, rels


def parse_schema_yaml(raw: str) -> tuple[dict[str, pd.DataFrame], list[dict]]:
    """Same as JSON but YAML input."""
    data = yaml.safe_load(raw)
    return parse_schema_json(json.dumps(data))


def table_digest(tables: dict, method: str) -> str:
    parts = "|".join(f"{k}:{len(v)}:{len(v.columns)}" for k, v in tables.items())
    return hashlib.md5(f"{parts}//{method}".encode()).hexdigest()


# ═══════════════════════════════════════════════════════════════════════════
# Network / Graph builder (pyvis via networkx)
# ═══════════════════════════════════════════════════════════════════════════

def build_pyvis_html(tables: dict, rels: list[dict], pk_map: dict,
                     cpk_map: dict | None = None,
                     spring_length: int = 220, dark_mode: bool = True) -> str:
    """
    Render an interactive vis.js network and return the HTML string.
    Tooltips are injected via a custom overlay div driven by JS to avoid
    pyvis escaping the HTML in the title attribute.
    """
    from pyvis.network import Network

    _bg      = "#141e30" if dark_mode else "#f1f5fb"
    _fg      = "#e8eef6" if dark_mode else "#0d1829"
    _node_bg = "#1e2d4a" if dark_mode else "#ffffff"
    _node_bd = "#2a3f60" if dark_mode else "#aabdd4"
    _node_hi = "#253858" if dark_mode else "#ddeeff"
    _node_hb = "#5badff" if dark_mode else "#1a62c7"
    _tip_bg  = "#1a2640" if dark_mode else "#ffffff"
    _tip_bd  = "#2a3f60" if dark_mode else "#ccd8ea"
    _muted   = "#9ab0cc" if dark_mode else "#475569"
    _dim     = "#5a7898" if dark_mode else "#94a3b8"
    _yellow  = "#fbbf24" if dark_mode else "#854d0e"

    net = Network(height="560px", width="100%", bgcolor=_bg, font_color=_fg,
                  directed=True, notebook=False)
    net.force_atlas_2based(gravity=-60, central_gravity=0.005, spring_length=spring_length,
                            spring_strength=0.04, damping=0.9)

    method_colors = {
        "schema":          "#5badff",
        "naming":          "#4ade80",
        "name_similarity": "#34d399",
        "value_overlap":   "#fb923c",
        "cardinality":     "#fbbf24",
        "format":          "#c084fc",
        "distribution":    "#5badff",
        "null_pattern":    "#f97316",
        "manual":          "#f87171",
        "content":         "#c084fc",
    }

    # Build tooltip HTML strings stored in a JS dict — bypasses pyvis title escaping
    node_tooltips = {}
    edge_tooltips = {}

    for tname, df in tables.items():
        pks = pk_map.get(tname, [])
        cpks = (cpk_map or {}).get(tname, [])
        fk_rels = [r for r in rels if r["from_table"] == tname]
        fk_str = "<br>".join(f"{r['from_col']} → {r['to_table']}" for r in fk_rels) or "none"
        if pks:
            pk_str = ", ".join(pks)
        elif cpks:
            pk_str = " | ".join(" + ".join(g) for g in cpks) + " (composite)"
        else:
            pk_str = "none detected"
        src = df.attrs.get("source", "csv")
        row_str = f"{len(df):,}" if len(df) > 0 else "schema"

        node_tooltips[tname] = (
            f"<div style='color:#5badff;font-size:13px;font-weight:700;margin-bottom:8px;'>{tname}</div>"
            f"<span style='color:#4ade80;'>■ {row_str} rows</span>&nbsp;&nbsp;"
            f"<span style='color:#5badff;'>■ {len(df.columns)} cols</span><br>"
            f"<div style='border-top:1px solid #243b58;margin:8px 0 4px;padding-top:6px;'>"
            f"<span style='color:{_muted};font-size:9px;letter-spacing:1px;'>PRIMARY KEY</span><br>"
            f"<span style='color:{_yellow};'>{pk_str}</span></div>"
            f"<div style='border-top:1px solid #243b58;margin:8px 0 4px;padding-top:6px;'>"
            f"<span style='color:{_muted};font-size:9px;letter-spacing:1px;'>FOREIGN KEYS</span><br>"
            f"<span style='color:#c084fc;'>{fk_str}</span></div>"
            f"<div style='border-top:1px solid #243b58;margin:8px 0 0;padding-top:6px;'>"
            f"<span style='color:{_muted};font-size:9px;letter-spacing:1px;'>SOURCE: {src.upper()}</span></div>"
        )

        label = f"{tname}\n{row_str} rows | {len(df.columns)} cols"
        # title=" " — single space so pyvis doesn't strip the title attr,
        # but blank enough that nothing shows if our overlay fails
        net.add_node(tname, label=label, title=" ",
                     color={"background": _node_bg, "border": _node_bd,
                            "highlight": {"background": _node_hi, "border": _node_hb}},
                     font={"color": _fg, "size": 13, "face": "JetBrains Mono"},
                     shape="box", shadow=True, widthConstraint={"minimum": 140, "maximum": 220})

    for i, r in enumerate(rels):
        ecol = method_colors.get(r["detected_by"], "#00d4ff")
        to_col = r.get("to_col") or "?"
        edge_id = f"e{i}"
        conf     = r.get("confidence", "")
        score    = r.get("score", "")
        reasons  = r.get("reasons", [])
        score_str = f" · {score:.0%}" if isinstance(score, float) else ""
        conf_str  = f"<br><span style='color:{_muted};font-size:9px;'>confidence: {conf}{score_str}</span>" if conf else ""
        reasons_str = ""
        if reasons:
            rlist = " · ".join(reasons[:4])
            reasons_str = f"<br><span style='color:{_muted};font-size:9px;'>{rlist}</span>"
        edge_tooltips[edge_id] = (
            f"<span style='color:{_muted};'>{r['from_table']}.</span>"
            f"<span style='color:{_yellow};font-weight:700;'>{r['from_col']}</span>"
            f"<span style='color:{_dim};'> → </span>"
            f"<span style='color:{_muted};'>{r['to_table']}.</span>"
            f"<span style='color:{_yellow};font-weight:700;'>{to_col}</span><br>"
            f"<span style='color:{ecol};font-size:9px;letter-spacing:1px;'>■ {r['detected_by'].replace('_',' ').upper()}</span>"
            f"{conf_str}{reasons_str}"
        )
        net.add_edge(r["from_table"], r["to_table"],
                     label=r["from_col"], title=" ",
                     id=edge_id,
                     color={"color": ecol, "highlight": ecol, "opacity": 0.85},
                     arrows="to", dashes=(r["detected_by"] in ("value_overlap","distribution","null_pattern","format")),
                     font={"color": "#cdd9e8", "size": 10, "face": "JetBrains Mono",
                           "strokeWidth": 3, "strokeColor": "#070d16"})

    net.set_options("""
    {
      "interaction": {"hover": true, "tooltipDelay": 80, "navigationButtons": true, "hideEdgesOnDrag": false},
      "physics": {"enabled": true, "stabilization": {"iterations": 300}},
      "layout": {"randomSeed": 42}
    }
    """)

    raw_html = net.generate_html(notebook=False)

    # ── Inject custom tooltip overlay + JS ───────────────────────────────
    node_tips_js = json.dumps(node_tooltips)
    edge_tips_js = json.dumps(edge_tooltips)

    custom_js = f"""
<style>
#custom-tooltip {{
  position: fixed;
  z-index: 9999;
  background: {_tip_bg};
  color: {_fg};
  font-family: 'JetBrains Mono', monospace;
  font-size: 11px;
  padding: 14px 16px;
  min-width: 210px;
  max-width: 320px;
  border: 1px solid {_tip_bd};
  border-radius: 8px;
  line-height: 1.8;
  box-shadow: 0 8px 32px rgba(0,0,0,0.4);
  pointer-events: none;
  display: none;
}}
</style>
<div id="custom-tooltip"></div>
<script>
(function() {{
  var nodeTips = {node_tips_js};
  var edgeTips = {edge_tips_js};
  var tip = document.getElementById('custom-tooltip');

  function showTip(html, x, y) {{
    tip.innerHTML = html;
    tip.style.display = 'block';
    // Keep tooltip inside viewport
    var vw = window.innerWidth, vh = window.innerHeight;
    var tw = 240, th = tip.offsetHeight || 180;
    var left = x + 16;
    var top  = y + 16;
    if (left + tw > vw) left = x - tw - 8;
    if (top  + th > vh) top  = y - th - 8;
    tip.style.left = left + 'px';
    tip.style.top  = top  + 'px';
  }}

  function hideTip() {{
    tip.style.display = 'none';
  }}

  // Poll until vis network is available then bind events
  var attempts = 0;
  var poll = setInterval(function() {{
    attempts++;
    if (attempts > 100) {{ clearInterval(poll); return; }}
    var canvas = document.querySelector('canvas');
    if (!canvas) return;
    // Find the vis Network instance on the window
    var net = null;
    for (var k in window) {{
      try {{
        if (window[k] && window[k].body && window[k].body.nodes) {{
          net = window[k]; break;
        }}
      }} catch(e) {{}}
    }}
    if (!net) return;
    clearInterval(poll);

    // ── Pin node in place after drag ──────────────────────────────────
    // When the user drops a node, fix its x/y so the physics engine
    // stops pulling it back toward the cluster.
    var dragging = false;

    net.on('dragStart', function(params) {{
      if (params.nodes.length > 0) {{
        dragging = true;
        hideTip();
      }}
    }});

    net.on('dragEnd', function(params) {{
      dragging = false;
      if (params.nodes.length > 0) {{
        var nodeId = params.nodes[0];
        var pos = net.getPositions([nodeId])[nodeId];
        // Fix the node at its dropped position — physics won't move it again
        net.body.data.nodes.update({{
          id: nodeId,
          x: pos.x,
          y: pos.y,
          fixed: {{ x: true, y: true }}
        }});
      }}
    }});

    // Double-click a node to unpin it (release back into physics)
    net.on('doubleClick', function(params) {{
      if (params.nodes.length > 0) {{
        var nodeId = params.nodes[0];
        net.body.data.nodes.update({{
          id: nodeId,
          fixed: {{ x: false, y: false }}
        }});
      }}
    }});

    // ── Tooltips ──────────────────────────────────────────────────────
    net.on('hoverNode', function(params) {{
      if (dragging) return;
      var nodeId = params.node;
      var html = nodeTips[nodeId];
      if (html) showTip(html, params.event.clientX, params.event.clientY);
    }});
    net.on('blurNode', hideTip);

    net.on('hoverEdge', function(params) {{
      if (dragging) return;
      var edgeId = params.edge;
      var html = edgeTips[edgeId];
      if (html) showTip(html, params.event.clientX, params.event.clientY);
    }});
    net.on('blurEdge', hideTip);

    // Update tooltip position on mouse move
    document.addEventListener('mousemove', function(e) {{
      if (dragging) {{ hideTip(); return; }}
      if (tip.style.display === 'block') {{
        var vw = window.innerWidth, vh = window.innerHeight;
        var tw = 240, th = tip.offsetHeight || 180;
        var left = e.clientX + 16;
        var top  = e.clientY + 16;
        if (left + tw > vw) left = e.clientX - tw - 8;
        if (top  + th > vh) top  = e.clientY - th - 8;
        tip.style.left = left + 'px';
        tip.style.top  = top  + 'px';
      }}
    }});
  }}, 100);
}})();
</script>
"""

    # Insert our overlay just before </body>
    raw_html = raw_html.replace("</body>", custom_js + "\n</body>")
    return raw_html


# ═══════════════════════════════════════════════════════════════════════════
# Session state initialisation
# ═══════════════════════════════════════════════════════════════════════════

if "dark_mode" not in st.session_state:
    st.session_state.dark_mode = True
if "tables" not in st.session_state:
    st.session_state.tables = {}          # name → pd.DataFrame
if "schema_rels" not in st.session_state:
    st.session_state.schema_rels = []     # from JSON/YAML schema
if "manual_rels" not in st.session_state:
    st.session_state.manual_rels = []
if "fk_cache" not in st.session_state:
    st.session_state.fk_cache = {}        # digest → rels list
if "last_digest" not in st.session_state:
    st.session_state.last_digest = ""
if "show_schema_ref" not in st.session_state:
    st.session_state.show_schema_ref = False
if "show_signal_toggles" not in st.session_state:
    st.session_state.show_signal_toggles = False


# ═══════════════════════════════════════════════════════════════════════════
# Header
# ═══════════════════════════════════════════════════════════════════════════

# ── Theme state ─────────────────────────────────────────────────────────
if "dark_mode" not in st.session_state:
    st.session_state.dark_mode = True




# ═══════════════════════════════════════════════════════════════════════════
# Sidebar
# ═══════════════════════════════════════════════════════════════════════════

with st.sidebar:
    # ── App title ────────────────────────────────────────────────────────
    st.markdown("""
<div style="padding:4px 12px 0; font-family:'JetBrains Mono',monospace;">
  <div style="font-size:9px;letter-spacing:3px;color:var(--accent);text-transform:uppercase;
              background:rgba(77,166,255,0.08);border:1px solid rgba(77,166,255,0.2);
              padding:4px 10px;border-radius:4px;display:inline-block;margin-bottom:8px;">◫ TRE</div>
  <div style="font-size:13px;font-weight:700;color:var(--text);letter-spacing:0.3px;margin-bottom:2px;">
    TABLE_RELATIONSHIP_EXPLORER</div>
  <div style="font-size:11px;color:var(--text3);font-family:'Inter',sans-serif;line-height:1.4;">
    Detect PKs &amp; FKs across CSV tables</div>
</div>""", unsafe_allow_html=True)

    # ── Theme toggle ─────────────────────────────────────────────────────
    st.markdown('<div style="height:6px;"></div>', unsafe_allow_html=True)
    toggle_label = "☀  Light mode" if st.session_state.dark_mode else "☾  Dark mode"
    if st.button(toggle_label, key="theme_btn", width='stretch'):
        st.session_state.dark_mode = not st.session_state.dark_mode
        st.rerun()
    st.markdown('<div style="height:4px;"></div>', unsafe_allow_html=True)

    # ── 01 Upload CSVs ───────────────────────────────────────────────────
    st.markdown('<div class="sidebar-section">01 // Upload Tables</div>', unsafe_allow_html=True)
    csv_files = st.file_uploader("Upload table files", type=["csv", "tsv", "xlsx", "xls", "xlsm", "ods", "parquet", "json", "ndjson"], accept_multiple_files=True, key="csv_upload", label_visibility="collapsed")

    if csv_files:
        added, replaced = 0, 0
        for f in csv_files:
            try:
                ext = Path(f.name).suffix.lower().lstrip(".")
                tname = Path(f.name).stem

                if ext in ("csv", "tsv"):
                    sep = "\t" if ext == "tsv" else ","
                    df = pd.read_csv(f, sep=sep)
                    fmt = ext
                elif ext in ("xlsx", "xlsm"):
                    xl = pd.ExcelFile(f, engine="openpyxl")
                    sheets = xl.sheet_names
                    if len(sheets) == 1:
                        df = xl.parse(sheets[0])
                        fmt = "xlsx"
                    else:
                        # Multi-sheet: load each sheet as its own table
                        for sheet in sheets:
                            sdf = xl.parse(sheet)
                            sdf.columns = [re.sub(r"[^a-zA-Z0-9_]", "_", c) for c in sdf.columns]
                            sname = f"{tname}_{sheet}"
                            existed = sname in st.session_state.tables
                            sdf.attrs["source"] = "xlsx"
                            st.session_state.tables[sname] = sdf
                            replaced += existed; added += not existed
                        continue
                elif ext == "xls":
                    df = pd.read_excel(f, engine="xlrd")
                    fmt = "xls"
                elif ext == "ods":
                    df = pd.read_excel(f, engine="odf")
                    fmt = "ods"
                elif ext == "parquet":
                    df = pd.read_parquet(f)
                    fmt = "parquet"
                elif ext == "json":
                    df = pd.read_json(f)
                    fmt = "json"
                elif ext == "ndjson":
                    df = pd.read_json(f, lines=True)
                    fmt = "ndjson"
                else:
                    st.error(f"Unsupported format: {f.name}")
                    continue

                df.columns = [re.sub(r"[^a-zA-Z0-9_]", "_", str(c)) for c in df.columns]
                existed = tname in st.session_state.tables
                df.attrs["source"] = fmt
                st.session_state.tables[tname] = df
                replaced += existed; added += not existed

            except Exception as e:
                st.error(f"Could not read {f.name}: {e}")

        parts = []
        if added:    parts.append(f"{added} added")
        if replaced: parts.append(f"{replaced} replaced")
        if parts:
            st.success(f"{', '.join(parts)} — {len(st.session_state.tables)} table(s) total")

    # ── 02 Schema Definition ─────────────────────────────────────────────
    st.markdown('<div class="sidebar-section">02 // Schema Definition (JSON/YAML)</div>', unsafe_allow_html=True)
    st.markdown('<div class="sidebar-hint">Define exact relationships without data. Overrides auto-detection for named tables.</div>', unsafe_allow_html=True)

    schema_file = st.file_uploader("Upload schema file", type=["json", "yaml", "yml"], key="schema_upload", label_visibility="collapsed")
    if schema_file:
        raw = schema_file.read().decode("utf-8")
        try:
            if schema_file.name.endswith(".json"):
                tbls, srels = parse_schema_json(raw)
            else:
                tbls, srels = parse_schema_yaml(raw)
            for tname, df in tbls.items():
                st.session_state.tables[tname] = df
            st.session_state.schema_rels = srels
            st.success(f"Schema loaded: {len(tbls)} table(s), {len(srels)} relationship(s)")
        except Exception as e:
            st.error(f"Schema parse error: {e}")

    _ref_label = "Hide schema reference" if st.session_state.show_schema_ref else "Show schema reference"
    if st.button(_ref_label, key="toggle_schema_ref", width='stretch'):
        st.session_state.show_schema_ref = not st.session_state.show_schema_ref
        st.rerun()
    if st.session_state.show_schema_ref:
        st.code("""{
  "tables": [
    {
      "name": "orders",
      "columns": [
        {"name": "order_id", "type": "integer", "primary_key": true},
        {"name": "customer_id", "type": "integer",
         "foreign_key": {"table": "customers", "column": "customer_id"}},
        {"name": "amount", "type": "numeric"}
      ]
    },
    {
      "name": "customers",
      "columns": [
        {"name": "customer_id", "type": "integer", "primary_key": true},
        {"name": "name", "type": "text"}
      ]
    }
  ]
}""", language="json")

    # ── Loaded tables ────────────────────────────────────────────────────
    if st.session_state.tables:
        st.markdown('<div class="sidebar-section">Loaded Tables</div>', unsafe_allow_html=True)
        to_remove = []
        for tname, df in st.session_state.tables.items():
            src = df.attrs.get("source", "csv")
            row_str = f"{len(df):,}r" if len(df) > 0 else "schema"
            col1, col2 = st.columns([4, 1])
            with col1:
                st.markdown(f"""<div class="tbl-item">
                  <span class="tn">{tname}</span>
                  <span class="tm">{row_str} × {len(df.columns)}c [{src}]</span>
                </div>""", unsafe_allow_html=True)
            with col2:
                if st.button("✕", key=f"rm_{tname}", help=f"Remove {tname}"):
                    to_remove.append(tname)
        for t in to_remove:
            del st.session_state.tables[t]
            st.rerun()

        if st.button("✕  Remove All", type="secondary", width='stretch'):
            st.session_state.tables = {}
            st.session_state.schema_rels = []
            st.session_state.manual_rels = []
            st.session_state.fk_cache = {}
            st.rerun()

    # ── 03 Detection Method ──────────────────────────────────────────────
    st.markdown('<div class="sidebar-section">03 // Detection Method</div>', unsafe_allow_html=True)
    detect_method = st.selectbox("Signal family", options=[
        ("All signals (recommended)", "both"),
        ("Naming conventions only",   "naming"),
        ("Content analysis only",     "content"),
        ("Manual only",               "manual"),
    ], format_func=lambda x: x[0], index=0)[1]

    min_confidence = st.select_slider(
        "Min confidence to show",
        options=["low", "medium", "high"],
        value="medium",
    )

    if detect_method not in ("manual", "naming"):
        _sig_label = "Hide signal toggles" if st.session_state.show_signal_toggles else "Signal toggles"
        if st.button(_sig_label, key="toggle_signals", width='stretch'):
            st.session_state.show_signal_toggles = not st.session_state.show_signal_toggles
            st.rerun()
        if st.session_state.show_signal_toggles:
            fl_overlap  = st.checkbox("Value overlap",            value=True,  key="fl_overlap")
            fl_card     = st.checkbox("Cardinality match",        value=True,  key="fl_card")
            fl_fmt      = st.checkbox("Format fingerprint",       value=True,  key="fl_fmt")
            fl_dist     = st.checkbox("Distribution similarity",  value=True,  key="fl_dist")
            fl_null     = st.checkbox("Null-pattern correlation", value=False, key="fl_null",
                                      help="Only useful when both tables have the same row count")
        else:
            fl_overlap = st.session_state.get("fl_overlap", True)
            fl_card    = st.session_state.get("fl_card",    True)
            fl_fmt     = st.session_state.get("fl_fmt",     True)
            fl_dist    = st.session_state.get("fl_dist",    True)
            fl_null    = st.session_state.get("fl_null",    False)
        enable_flags = {
            "naming":        detect_method in ("naming", "both"),
            "value_overlap": fl_overlap,
            "cardinality":   fl_card,
            "format":        fl_fmt,
            "distribution":  fl_dist,
            "null_pattern":  fl_null,
        }
    else:
        enable_flags = {
            "naming":        detect_method in ("naming", "both"),
            "value_overlap": False, "cardinality": False,
            "format":        False, "distribution": False, "null_pattern": False,
        }

    enable_composite_pk = st.checkbox(
        "Detect composite keys",
        value=False,
        key="enable_composite_pk",
        help=(
            "When no single-column primary key is found, try combinations "
            "of 2–3 columns whose values together uniquely identify each row."
        ),
    )

    # ── 04 Manual Override ───────────────────────────────────────────────
    st.markdown('<div class="sidebar-section">04 // Manual Override</div>', unsafe_allow_html=True)
    tnames = list(st.session_state.tables.keys())

    if len(tnames) >= 2:
        m_from_t = st.selectbox("From Table", tnames, key="m_from_t")
        from_cols = list(st.session_state.tables[m_from_t].columns) if m_from_t else []
        m_from_c = st.selectbox("From Column", from_cols, key="m_from_c")

        other_tables = [t for t in tnames if t != m_from_t]
        m_to_t = st.selectbox("To Table", other_tables, key="m_to_t")
        to_cols = list(st.session_state.tables[m_to_t].columns) if m_to_t else []
        m_to_c = st.selectbox("To Column (PK)", to_cols, key="m_to_c")

        if st.button("＋ Add Relationship", type="primary", width='stretch'):
            st.session_state.manual_rels.append(dict(
                from_table=m_from_t, from_col=m_from_c,
                to_table=m_to_t, to_col=m_to_c, detected_by="manual"))
            st.success("Relationship added.")

        if st.button("✕  Clear Manual", type="secondary", width='stretch'):
            st.session_state.manual_rels = []
            st.rerun()
    else:
        st.markdown('<div class="sidebar-hint">Load at least 2 tables to add manual relationships.</div>', unsafe_allow_html=True)


# ═══════════════════════════════════════════════════════════════════════════
# Main Panel
# ═══════════════════════════════════════════════════════════════════════════

tables = st.session_state.tables
st.markdown('<div style="padding: 0 20px;">', unsafe_allow_html=True)

if not tables:
    st.markdown("""
    <div class="empty-state">
      <div class="icon">◫</div>
      <h3>No tables loaded</h3>
      <p style="color:#5a7898;font-size:13px;font-family:'JetBrains Mono',monospace;">
        Upload CSV files or a JSON/YAML schema using the sidebar to begin.
      </p>
    </div>
    """, unsafe_allow_html=True)
else:
    # Compute PKs
    method = detect_method
    pk_method = "both" if method == "manual" else method
    pk_map = {t: detect_pks(df, t, pk_method) for t, df in tables.items()}

    # Compute composite PKs (optional, off by default)
    cpk_map: dict[str, list[list[str]]] = {}
    if enable_composite_pk:
        cpk_map = {t: detect_composite_pks(df, t) for t, df in tables.items()}

    # Compute FK rels (cached by digest that includes all detection params)
    flags_key = json.dumps(enable_flags, sort_keys=True)
    digest = table_digest(tables, f"{method}/{min_confidence}/{flags_key}/{st.session_state.get('dark_mode', True)}")
    if digest not in st.session_state.fk_cache:
        with st.spinner("Analysing tables…"):
            auto_rels = detect_fks(
                tables,
                method=method,
                min_confidence=min_confidence,
                enable_flags=enable_flags,
            )
        st.session_state.fk_cache[digest] = auto_rels
        st.session_state.last_digest = digest
    else:
        auto_rels = st.session_state.fk_cache[digest]

    # Merge: schema > auto > manual (schema rels skip auto for those table pairs)
    schema_pairs = {(r["from_table"], r["to_table"]) for r in st.session_state.schema_rels}
    filtered_auto = [r for r in auto_rels if (r["from_table"], r["to_table"]) not in schema_pairs]
    all_rels = st.session_state.schema_rels + filtered_auto + st.session_state.manual_rels

    # ── Tabs ─────────────────────────────────────────────────────────────
    tab_erd, tab_details, tab_rels = st.tabs(["ERD Diagram", "Table Details", "Relationships"])

    # ─── ERD ─────────────────────────────────────────────────────────────
    with tab_erd:
        erd_top_left, erd_top_right = st.columns([3, 1])
        with erd_top_left:
            st.markdown("""
            <div class="legend">
              <div class="li"><div class="ld" style="background:#00e5a0;"></div>naming</div>
              <div class="li"><div class="ld" style="background:#ff7b35;border-style:dashed;"></div>uniqueness (dashed)</div>
              <div class="li"><div class="ld" style="background:#ff4d6d;"></div>manual</div>
              <div class="li"><div class="ld" style="background:#00d4ff;"></div>schema</div>
            </div>
            """, unsafe_allow_html=True)
        with erd_top_right:
            spring_length = st.slider(
                "Node spacing",
                min_value=80, max_value=600, value=220, step=20,
                help="Increase to spread nodes further apart"
            )

        try:
            html = build_pyvis_html(tables, all_rels, pk_map, cpk_map=cpk_map, spring_length=spring_length, dark_mode=st.session_state.get('dark_mode', True))
            st.components.v1.html(html, height=580, scrolling=False)
            st.markdown('<div class="erd-hint">drag to move &amp; pin nodes · double-click to unpin · scroll to zoom · hover for details</div>', unsafe_allow_html=True)
        except Exception as e:
            st.error(f"ERD render error: {e}")

    # ─── Table Details ────────────────────────────────────────────────────

    def _pk_label(col: str, single_pks: list[str], composite_cols: set[str]) -> str:
        if col in single_pks:
            return "✓"
        if col in composite_cols:
            return "CPK"
        return ""

    with tab_details:
        for tname, df in tables.items():
            pks = pk_map.get(tname, [])
            cpks = cpk_map.get(tname, [])   # list of column-name lists
            fk_rels = [r for r in all_rels if r["from_table"] == tname]
            fk_cols = [r["from_col"] for r in fk_rels]
            src = df.attrs.get("source", "csv")

            # Flat set of all columns that are part of any composite key
            cpk_cols: set[str] = {c for group in cpks for c in group}

            # Pills HTML
            pills = f'<span class="pill p-rows">{len(df):,} rows</span>' if len(df) > 0 else f'<span class="pill p-schema">schema only</span>'
            pills += f'<span class="pill p-cols">{len(df.columns)} cols</span>'
            pills += f'<span class="pill p-schema">{src}</span>'
            if pks:
                for p in pks:
                    pills += f'<span class="pill p-pk">PK: {p}</span>'
            elif cpks:
                for group in cpks:
                    label = " + ".join(group)
                    pills += f'<span class="pill p-pk">CPK: {label}</span>'
            else:
                pills += '<span class="pill p-warn">⚠ no PK</span>'
            for r in fk_rels:
                pills += f'<span class="pill p-fk">FK: {r["from_col"]} → {r["to_table"]}</span>'

            st.markdown(f"""<div class="tbl-card">
              <div class="tbl-card-title">[ {tname} ] <span class="src-badge">{src.upper()}</span></div>
              <div style="margin-bottom:14px;">{pills}</div>
            </div>""", unsafe_allow_html=True)

            # Column summary table
            if len(df) > 0:
                summary = pd.DataFrame({
                    "Column": df.columns,
                    "Type": [str(df[c].dtype) for c in df.columns],
                    "Non-null": [df[c].notna().sum() for c in df.columns],
                    "Unique": [df[c].nunique() for c in df.columns],
                    "PK": [_pk_label(c, pks, cpk_cols) for c in df.columns],
                    "FK": ["✓" if c in fk_cols else "" for c in df.columns],
                })
            else:
                # Schema-only table — use column metadata if available
                meta = df.attrs.get("columns_meta", [])
                if meta:
                    summary = pd.DataFrame({
                        "Column": [c["name"] for c in meta],
                        "Type": [c.get("type", "unknown") for c in meta],
                        "Non-null": ["—"] * len(meta),
                        "Unique": ["—"] * len(meta),
                        "PK": ["✓" if c.get("primary_key") else "" for c in meta],
                        "FK": ["✓" if c.get("foreign_key") else "" for c in meta],
                    })
                else:
                    summary = pd.DataFrame({
                        "Column": list(df.columns),
                        "Type": ["—"] * len(df.columns),
                        "Non-null": ["—"] * len(df.columns),
                        "Unique": ["—"] * len(df.columns),
                        "PK": [_pk_label(c, pks, cpk_cols) for c in df.columns],
                        "FK": ["✓" if c in fk_cols else "" for c in df.columns],
                    })

            st.dataframe(summary, width='stretch', hide_index=True, height=min(len(summary) * 35 + 50, 320))

    # ─── Relationships ────────────────────────────────────────────────────
    with tab_rels:
        if not all_rels:
            st.markdown("""<div class="empty-state">
              <div class="icon">⇌</div>
              <h3>No relationships detected</h3>
              <p style="color:#5a7898;font-size:13px;font-family:'JetBrains Mono',monospace;">
                Try uploading more tables, adjusting detection settings, or lowering the confidence threshold.
              </p>
            </div>""", unsafe_allow_html=True)
        else:
            ALL_SECTIONS = [
                ("schema",          "Schema-Defined",          "m-schema"),
                ("naming",          "Naming Convention",       "m-naming"),
                ("name_similarity", "Name Similarity",         "m-name_similarity"),
                ("value_overlap",   "Value Overlap",           "m-value_overlap"),
                ("cardinality",     "Cardinality Match",       "m-cardinality"),
                ("format",          "Format Fingerprint",      "m-format"),
                ("distribution",    "Distribution Similarity", "m-distribution"),
                ("null_pattern",    "Null-Pattern Correlation","m-null_pattern"),
                ("manual",          "Manual Override",         "m-manual"),
            ]
            for key, label, cls in ALL_SECTIONS:
                sec_rels = [r for r in all_rels if r["detected_by"] == key]
                if not sec_rels:
                    continue
                st.markdown(f'<div class="rel-section">{label} ({len(sec_rels)})</div>', unsafe_allow_html=True)
                for r in sec_rels:
                    to_col   = r.get("to_col") or "?"
                    conf     = r.get("confidence", "")
                    score    = r.get("score", "")
                    reasons  = r.get("reasons", [])
                    signals  = r.get("signals", {})

                    # Confidence block (only for auto-detected rels)
                    if conf:
                        score_str = f"{score:.0%}" if isinstance(score, float) else ""
                        chips_html = "".join(
                            f'<span class="sig-chip">{s.replace("_"," ")}</span>'
                            for s in signals
                        )
                        conf_html = f"""<div class="conf-bar conf-{conf}">
                          <div class="conf-dot"></div>
                          <span class="conf-label">{conf}</span>
                          <span class="conf-score">{score_str}</span>
                        </div>"""
                        chips_block = f'<div class="signal-chips">{chips_html}</div>' if chips_html else ""
                    else:
                        conf_html = ""
                        chips_block = ""

                    st.markdown(f"""<div class="rel-row" style="flex-wrap:wrap;">
                      <span class="rt">{r["from_table"]}</span>
                      <span class="rc">.{r["from_col"]}</span>
                      <span class="ra">→</span>
                      <span class="rt">{r["to_table"]}</span>
                      <span class="rc">.{to_col}</span>
                      <span class="rm {cls}">{key.replace("_"," ")}</span>
                      {conf_html}
                      {chips_block}
                    </div>""", unsafe_allow_html=True)

        st.markdown('<div style="height:16px;"></div>', unsafe_allow_html=True)

        # Export (include confidence + signals)
        if all_rels:
            export_df = pd.DataFrame([{
                "from_table":  r["from_table"],
                "from_col":    r["from_col"],
                "to_table":    r["to_table"],
                "to_col":      r.get("to_col") or "",
                "detected_by": r["detected_by"],
                "confidence":  r.get("confidence", ""),
                "score":       r.get("score", ""),
                "signals":     ", ".join(r.get("signals", {}).keys()),
                "reasons":     "; ".join(r.get("reasons", [])),
            } for r in all_rels])

            csv_bytes = export_df.to_csv(index=False).encode()
            st.download_button(
                label="⬇  Export Relationships CSV",
                data=csv_bytes,
                file_name="table_relationships.csv",
                mime="text/csv",
            )

st.markdown('</div>', unsafe_allow_html=True)
