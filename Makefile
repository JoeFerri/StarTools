# ------------------------------------------------------------
# MSYS2 UCRT64 (Windows 10)
# WSL2 Ubuntu 24.04 (Windows 10/11)
#
# Makefile for generating PasDoc + Graphviz documentation
# ------------------------------------------------------------


# =================================================================
# WSL ENVIRONMENTAL MONITORING
# =================================================================

OSRELEASE := $(shell cat /proc/sys/kernel/osrelease 2>/dev/null)
VERSION   := $(shell cat /proc/version 2>/dev/null)

IS_WSL_OSRELEASE := $(findstring microsoft,$(OSRELEASE))
IS_WSL_VERSION   := $(findstring microsoft,$(VERSION))

ifeq ($(strip $(IS_WSL_OSRELEASE)$(IS_WSL_VERSION)),)
    PASDOC_P  = pasdoc
    DOT_P     = dot
    BUILD_ENV = Native/Linux

    SEP = /
    PATH_ROOT_SRC = /
    PATH_ROOT_OPT = /
else
    PASDOC_P  = pasdoc.exe
    DOT_P     = dot.exe
    BUILD_ENV = WSL/Windows

    # Obtains the Linux CWD (e.g. /mnt/c/Users/...) and converts it to Windows format (e.g. C:\Users\...)
    PATH_WIN_ROOT := $(shell wslpath -w "$$(pwd)")

    SEP = \\

    PATH_ROOT_SRC = $(PATH_WIN_ROOT)
    PATH_ROOT_OPT = $(PATH_WIN_ROOT)
endif

$(info Environment detected: $(BUILD_ENV))
$(info PasDoc executable: $(PASDOC_P))
$(info Dot executable: $(DOT_P))
$(info Windows root path: $(PATH_WIN_ROOT))


# =================================================================
# PROJECT VERSION
# =================================================================

# Check if the project is a prerelease version
IS_PRERELEASE := $(shell grep "Attributes pvaPreRelease=\"True\"" project/StarTools.lpi)
PRERELEASE := $(if $(IS_PRERELEASE),-alpha,)

# Extract versions with fallback to 0 if the tag is missing
MAJOR := $(shell grep "MajorVersionNr Value" project/startools.lpi | sed 's/[^0-9]*//g' || echo 0)
MINOR := $(shell grep "MinorVersionNr Value" project/startools.lpi | sed 's/[^0-9]*//g' || echo 0)
REVIS := $(shell grep "RevisionNr Value" project/startools.lpi | sed 's/[^0-9]*//g' || echo 0)

# If the grep command fails or the result is empty, set 0 as default value
MAJOR := $(if $(MAJOR),$(MAJOR),0)
MINOR := $(if $(MINOR),$(MINOR),0)
REVIS := $(if $(REVIS),$(REVIS),0)

VERSION_STR := $(MAJOR).$(MINOR).$(REVIS)$(PRERELEASE)


# =================================================================
# ZIP FILES
# =================================================================

BIN_DIR = bin
DATA_SRC = project/data
SAVES_SRC = project/saves
DOC_FILES = README.md LICENSE CHANGELOG.md

ZIP_RELEASE = $(BIN_DIR)/StarTools_v$(VERSION_STR)-Release.zip
ZIP_DEBUG   = $(BIN_DIR)/StarTools_v$(VERSION_STR)-Debug.zip


# =================================================================
# OPERATIONS
# =================================================================

OUTDIR = docs
OUTDIR_WIN = $(PATH_ROOT_SRC)$(SEP)$(OUTDIR)

DOT_CLASSES = $(OUTDIR)/GVClasses.dot
DOT_USES    = $(OUTDIR)/GVUses.dot
PNG_CLASSES = $(OUTDIR)/GVClasses.png
PNG_USES    = $(OUTDIR)/GVUses.png

DOT_CLASSES_WIN = $(PATH_ROOT_SRC)$(SEP)$(OUTDIR)/GVClasses.dot
DOT_USES_WIN    = $(PATH_ROOT_SRC)$(SEP)$(OUTDIR)/GVUses.dot
PNG_CLASSES_WIN = $(PATH_ROOT_SRC)$(SEP)$(OUTDIR)/GVClasses.png
PNG_USES_WIN    = $(PATH_ROOT_SRC)$(SEP)$(OUTDIR)/GVUses.png

SRC = \
    "$(PATH_ROOT_SRC)$(SEP)project/src/consoleoptionsunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/consolesettingsdialogunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/contractdbunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/contractunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/contractviewunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/infounit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/initunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/iounit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/mainserviceunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/mainunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/panelrowunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/scuxsizeformunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/scuxsizeunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/sizedialogunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/splashscreenunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/trlsortunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/usernicknamesunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/versionunit.pas" \
    "$(PATH_ROOT_SRC)$(SEP)project/src/webunit.pas"



CSS    = style/pasdoc_custom.css
FOOTER = style/footer.html
AGPL_ICON = agplv3-with-text-100x42.png
AGPL_ICON_SOURCE = style/$(AGPL_ICON)
EXTERNAL_CH = external_class_hierarchy.txt

CSS_WIN    = $(PATH_ROOT_OPT)$(SEP)$(CSS)
FOOTER_WIN = $(PATH_ROOT_OPT)$(SEP)$(FOOTER)
EXTERNAL_CH_WIN = $(PATH_ROOT_OPT)$(SEP)$(EXTERNAL_CH)


DEFINES = --define FPC --define MSWINDOWS

PASDOC_OPTS = \
    --format html \
    --output "$(OUTDIR_WIN)" \
    --markdown \
    --auto-abstract \
    --marker=* \
    --link-gv-uses png \
    --link-gv-classes png \
    --graphviz-uses \
    --graphviz-classes \
    --verbosity 2 \
    --exclude-generator \
    --title "StarTools Docs" \
    --footer "$(FOOTER_WIN)" \
    --css "$(CSS_WIN)" \
    --external-class-hierarchy="$(EXTERNAL_CH_WIN)"


$(OUTDIR):
	mkdir $(OUTDIR)

# PasDoc generates documentation + DOT files
doc: $(OUTDIR) $(FOOTER) $(CSS) 
	$(PASDOC_P) $(DEFINES) $(PASDOC_OPTS) $(SRC)
	cp "$(AGPL_ICON_SOURCE)" "$(OUTDIR)/$(AGPL_ICON)"

$(DOT_CLASSES): doc
$(DOT_USES): doc

# Convert DOT -> PNG with Graphviz
$(PNG_CLASSES): $(DOT_CLASSES)
	$(DOT_P) -Tpng "$(DOT_CLASSES_WIN)" -o "$(PNG_CLASSES_WIN)"

$(PNG_USES): $(DOT_USES)
	$(DOT_P) -Tpng "$(DOT_USES_WIN)" -o "$(PNG_USES_WIN)"

graphs: $(PNG_CLASSES) $(PNG_USES)


full-doc: doc graphs


dist:
	@if [ ! -f "project/startools-release.exe" ]; then echo "Error: Release executable not found! Build it in Lazarus first."; exit 1; fi
	@if [ ! -f "project/startools-debug.exe" ]; then echo "Error: Debug executable not found! Build it in Lazarus first."; exit 1; fi
	
	@echo "Cleaning up old packages..."
	mkdir -p $(BIN_DIR)
	@# Removes only the zip files of the current version to avoid conflicts or "ghost files"
	rm -f $(ZIP_RELEASE) $(ZIP_DEBUG)
	
	@echo "Packaging version $(VERSION_STR)..."
	
	# Release package creation
	@rm -rf temp_dist
	mkdir -p temp_dist/data temp_dist/saves
	cp -r $(DATA_SRC)/* temp_dist/data/
	cp $(DOC_FILES) temp_dist/
	cp project/startools-release.exe temp_dist/StarTools.exe
	cd temp_dist && zip -r ../$(ZIP_RELEASE) .
	
	# Debug package creation
	@rm -rf temp_dist
	mkdir -p temp_dist/data temp_dist/saves
	cp -r $(DATA_SRC)/* temp_dist/data/
	cp $(DOC_FILES) temp_dist/
	cp project/startools-debug.exe temp_dist/StarTools.exe
	cd temp_dist && zip -r ../$(ZIP_DEBUG) .
	
	@rm -rf temp_dist
	@echo "Packages created in $(BIN_DIR):"
	@ls -lh $(BIN_DIR)


all: full-doc dist


.PHONY: all clean doc graphs dist


clean:
	rm -rf $(OUTDIR)
