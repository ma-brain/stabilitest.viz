# stabilitest brand — "Clinical calm"

Shared by the `stabilitest` package and the `stabilitest.viz` app ("family of two").

## Logo — **selected: B · balance** (2026-10-26)

Final assets in `final/` (wordmark outlined, no font dependency):
- `stabilitest-hex.svg` / `stabilitest.viz-hex.svg` (+ PNG 240/480/1200 px; 240 px = `man/figures/logo.png` size)
- `lockup-light.svg` / `lockup-dark.svg` — horizontal navbar logo (simplified 4-dot mark)
- `favicon.svg` (+ PNG 32/180/512), `mark-light.svg`
- `logo-sheet.png` — overview

### Original concepts (archive, `concepts/`)
| Concept | Idea | Files |
|---|---|---|
| A · keystone | Arch with the keystone lifted out — does the structure still stand without one piece? | `A_keystone_*.svg` |
| B · balance | Beam of observations with one lifted (leave-one-out); the beam stays level | `B_balance_*.svg` |
| C · path | Removal path of p-values climbing towards the dashed α line | `C_path_*.svg` |

Family rule: **stabilitest** = ink hex, light mark. **stabilitest.viz** = paper hex, teal mark, amber `.viz` suffix.
Amber is reserved for the "removed / perturbed" element of the mark.

## Core palette
| Token | Hex | Use |
|---|---|---|
| Ink | `#14323A` | Text, package hex background |
| Teal | `#1F6F78` | Primary (buttons, links, active state) |
| Teal light | `#5FA8A6` | Secondary series, hover |
| Mist | `#E3EEEC` | Subtle fills, selected rows |
| Paper | `#F7F5F0` | Page background (light) |
| Sand | `#D9CFBF` | Borders, dividers |
| Amber | `#C8843A` | Accent graphic (logo, highlights, flip point) |
| Amber text | `#9A5E22` | Accent when used as text on light (AA) |

## Semantic (results)
Used **only** when stabilitest returns a validated label.
| Token | Light | Dark |
|---|---|---|
| Not fragile | `#2F7D5B` | `#6BC49A` |
| Fragile | `#B4483C` | `#E8857A` |
| Numeric only (fail-closed) | `#5E6E73` | `#9DB0B2` |

## Theme tokens (bslib)
| Token | Light | Dark |
|---|---|---|
| bg | `#F7F5F0` | `#0F1E22` |
| surface (cards) | `#FFFFFF` | `#16292E` |
| fg | `#14323A` | `#E8EEEC` |
| muted | `#5A6B70` | `#9DB0B2` |
| primary | `#1F6F78` | `#6CB8B3` |
| accent | `#9A5E22` | `#E0A262` |
| border | `#D9CFBF` | `#2A4148` |

All text tokens pass WCAG AA (≥ 4.5:1) on bg and surface in both modes.
White on primary 5.8:1 (light); ink on primary 7.4:1 (dark).

## Typography
- Wordmark: Inter Display SemiBold (outlined in the SVGs).
- UI: Inter (400 body, 500 labels, 600 headings) — `bslib::font_google(local = TRUE)`.
- Numbers in value boxes: Inter with tabular figures (`font-variant-numeric: tabular-nums`).
- Code: JetBrains Mono.

## Data-viz series order
Teal `#1F6F78` → Amber `#C8843A` → Teal light `#5FA8A6` → Ink `#14323A` → Sand `#B8A88F`.
Reference lines (α): dashed, muted. Observed value: amber.
