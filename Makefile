GHC=ghc
GHC_OPTS=-isrc -odir build -hidir build
BUILD_DIR=build
BIN=weather
TEST_BIN=run_tests

all: build

build:
	mkdir -p $(BUILD_DIR)
	TMPDIR=. $(GHC) $(GHC_OPTS) app/Main.hs -o $(BIN)

test:
	mkdir -p $(BUILD_DIR)
	TMPDIR=. $(GHC) $(GHC_OPTS) test/Tests.hs -o $(TEST_BIN)
	./$(TEST_BIN)

run: build
	./$(BIN)

clean:
	rm -rf $(BUILD_DIR) $(BIN) $(TEST_BIN) *.o *.hi run_tests run_tests.exe weather.exe
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete

.PHONY: all build test run clean
