# Changelog

All notable changes to `StarTools` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- About Window
    - Changelog section
    - Credits section
- Main Dashboard Window
    - Klescher Rehabilitation Facilities Timer
- Hauling Manager Window
    - Total SCUs for all routes filter buttons (for show/hide, group, ...)


### Changed
- label: "Total SCUs for all" -> "SCU breakdown by container size" in Totals Panel

## [1.1.0] - 2026-01-08

### Changed
- New font for the entire application: Electrolize (matching the Citizen Dossier page on robertsspaceindustries.com)
- Main Dashboard Window
    - Redesigned StarTools title with Electrolize font
    - Replaced timer icons
        - Current Time now uses clock icons
        - Star Citizen Online/Offline status uses game-specific icons
    - Uptime now tracks Star Citizen activity instead of StarTools, only when the game is running
    - Changed "Cargo Routes" button text to "Hauling Manager"

### Added
- Player Avatar image support
    - Default avatar image
    - Implemented custom image loading from the user's Citizen Dossier
- Player Organization Logo support
    - Default organization logo
    - Implemented custom logo loading from the user's Citizen Dossier
- Profile icon in the top menu
    - Opens a window to set the player's in-game Nickname
    - Submitting an empty field resets the Nickname link
- Tools item in the top menu
    - Added "Clear Cache" sub-item (clears local cache, including logos and avatars)
- Help item in the top menu
    - Added "About" sub-item to display program information
- About Window
    - Version section (title, logo, version)
    - Information section (license summary, external copyrights, and GitHub repository links)
- Implemented data fetching from the Citizen Dossier website using the player's Nickname
- Code optimization and cleanup
- Added new support units
- Code reorganization (VersionUnit for version management, IOUnit for directory handling, etc.)

## [1.0.0] - 2026-01-03

### Added
- Main Dashboard Window
    - STAR TOOLS title
    - Minimalist clock
    - Uptime tracker (for the StarTools application)
    - Cargo Routes button (opens the Cargo Routes window)
    - Top text menu
        - Settings -> Console Settings (opens console configuration)
    - Top icon menu
        - Terminal Test (debug)
        - Show/Hide Console
        - Always Show on Top (currently disabled due to bugs)
        - AGPL3 logo
    - Console configuration window
        - Window position settings:
            - System Default
            - Follow Main Window (Centered)
            - Monitor selection (numbered displays)
- Cargo Routes Window
    - Top text menu
        - File -> Load/Save (load/save from file)
        - Settings -> Reload Data List
        - Settings -> Open stations.list / commodities.list
        - Settings -> Console Settings
    - Top icon menu
        - Terminal Test (debug)
        - Show/Hide Console
        - Always Show on Top
        - Reset all panels to Default
        - Custom sorting (placeholder)
        - Show/Hide completed panels
        - Show/Hide contract details
    - Totals Panel
        - Total To Do (total SCUs remaining, including generic routes)
        - Total Done (total SCUs completed, including generic routes)
        - Total All SCU (sum of all SCUs, including generic routes)
        - Total SCUs for all routes (specific fields for each container size)
    - Add/Delete panel buttons
    - Implemented generic route concept using TPanelRow
        - Group ID handling: 0 for generic routes (limbo), n for specific contracts
            - "Delete all groups" icon: clears all contracts and resets groups to 0
            - "Order" icon: sort contracts by group
        - Loading/Unloading Station fields
            - "Delete all stations" icon: clears all contracts and loading stations
            - "Order" icon: sort contracts by loading station
        - Commodities field for transported materials/goods
        - SCU and Max Container Size fields
        - "Done" status and "Show/Hide" panel toggles
        - Detailed SCU breakdown fields (32, 24, 16, 8, 4, 2, 1) for each container size
    - Contract View window
        - Minimal text-only display for contract details
        - Save button: exports contract details to a plain text file