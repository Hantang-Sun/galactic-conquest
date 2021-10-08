BUILD_DIR = .tmp
OUT_DIR = out

$(BUILD_DIR) :
	mkdir -p $(BUILD_DIR)

$(OUT_DIR) :
	mkdir -p $(OUT_DIR)

# server
.PHONY: server
server: $(BUILD_DIR) $(OUT_DIR)
	ghc -O2 Main.hs -DBUILD_DIR=\"$(BUILD_DIR)\" -DOUT_DIR=\"$(OUT_DIR)\" -outputdir $(BUILD_DIR)

test_results_info.json: AutoTest.hs .FORCE
	runghc -iclever-ai AutoTest.hs > test_results_info.json

.PHONY: test
test: server test_results_info.json

.PHONY: .FORCE
.FORCE:
