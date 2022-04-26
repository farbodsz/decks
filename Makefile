.PHONY: zip

OUT_DIR := out
ZIP_LOC := $(OUT_DIR)/decks-code-dist.zip
EXCLUDED_DIRS := ./.git/\* ./cli/.stack-work/\* ./examples/node_modules/\* ./web/dist/\* ./web/node_modules/\*

zip:
	@rm -f $(ZIP_LOC)
	@mkdir -p $(OUT_DIR)
	zip -9 -r $(ZIP_LOC) . -x $(EXCLUDED_DIRS)
