# Makefile for ElevenLabs TTS Emacs package

# Configuration
EMACS ?= emacs
PACKAGE_NAME = elevenlabs-tts
PACKAGE_FILE = $(PACKAGE_NAME).el
TEST_FILE = test-$(PACKAGE_NAME).el
CONFIG_DIR = $(HOME)/.config/emacs-tts
INSTALL_DIR = $(HOME)/.emacs.d/emacs-tts
API_KEY_FILE = $(CONFIG_DIR)/elevenlabs-api-key

# Test configuration
TEST_PHRASE = "Hello, this is a test of the ElevenLabs text to speech system. Testing one, two, three."
TEST_OUTPUT = test-tts-output.mp3

# Colors for output
GREEN = \033[0;32m
YELLOW = \033[1;33m
RED = \033[0;31m
NC = \033[0m # No Color

.PHONY: all test test-unit test-integration test-manual install clean check-api-key setup help lint info

# Default target
all: help

# Help target
help:
	@echo "$(GREEN)ElevenLabs TTS Emacs Package$(NC)"
	@echo ""
	@echo "Available targets:"
	@echo "  $(YELLOW)help$(NC)          - Show this help message"
	@echo "  $(YELLOW)install$(NC)       - Install the package (create directories, copy files)"
	@echo "  $(YELLOW)setup$(NC)         - Setup configuration directory and API key file template"
	@echo "  $(YELLOW)test$(NC)          - Run all tests (unit + integration)"
	@echo "  $(YELLOW)test-unit$(NC)     - Run unit tests only"
	@echo "  $(YELLOW)test-integration$(NC) - Run integration tests (requires API key)"
	@echo "  $(YELLOW)test-manual$(NC)   - Generate a test audio file (requires API key)"
	@echo "  $(YELLOW)check-api-key$(NC) - Check if API key is configured"
	@echo "  $(YELLOW)lint$(NC)          - Lint Emacs Lisp code"
	@echo "  $(YELLOW)info$(NC)          - Show package information"
	@echo "  $(YELLOW)clean$(NC)         - Clean up test files"
	@echo ""
	@echo "$(GREEN)Installation:$(NC)"
	@echo "  1. Run 'make setup' to create config directory"
	@echo "  2. Add your API key to $(API_KEY_FILE)"
	@echo "  3. Run 'make install' to install the package"
	@echo "  4. Add (require 'elevenlabs-tts) to your Emacs config"

# Setup configuration directory and API key template
setup:
	@echo "$(GREEN)Setting up ElevenLabs TTS configuration...$(NC)"
	@mkdir -p $(CONFIG_DIR)
	@if [ ! -f "$(API_KEY_FILE)" ]; then \
		echo "your_elevenlabs_api_key_here" > $(API_KEY_FILE).example; \
		echo "$(YELLOW)Created API key template: $(API_KEY_FILE).example$(NC)"; \
		echo "$(RED)Please copy this file to $(API_KEY_FILE) and add your actual API key$(NC)"; \
	else \
		echo "$(GREEN)API key file already exists: $(API_KEY_FILE)$(NC)"; \
	fi
	@echo "$(GREEN)Configuration setup complete!$(NC)"

# Install the package
install: setup
	@echo "$(GREEN)Installing ElevenLabs TTS package...$(NC)"
	@mkdir -p $(INSTALL_DIR)
	@cp $(PACKAGE_FILE) $(INSTALL_DIR)/
	@cp $(TEST_FILE) $(INSTALL_DIR)/
	@cp README.md $(INSTALL_DIR)/
	@cp LICENSE $(INSTALL_DIR)/ 2>/dev/null || true
	@echo "$(GREEN)Package installed to: $(INSTALL_DIR)$(NC)"
	@echo ""
	@echo "$(YELLOW)Next steps:$(NC)"
	@echo "1. Add your ElevenLabs API key to: $(API_KEY_FILE)"
	@echo "2. Add this to your Emacs config:"
	@echo "   (add-to-list 'load-path \"$(INSTALL_DIR)\")"
	@echo "   (require 'elevenlabs-tts)"
	@echo "   (elevenlabs-tts-setup-keybinding)"
	@echo ""
	@echo "$(YELLOW)To override the default keybinding (C-c s), add your own:$(NC)"
	@echo "   ;; Example: Use C-c t instead of C-c s"
	@echo "   (global-set-key (kbd \"C-c t\") 'elevenlabs-tts-speak-selection)"
	@echo "   (global-set-key (kbd \"C-c T\") (lambda () (interactive) (elevenlabs-tts-speak-selection-quick 'male)))"

# Check if API key is configured
check-api-key:
	@if [ -f "$(API_KEY_FILE)" ]; then \
		if [ -s "$(API_KEY_FILE)" ] && ! grep -q "your_elevenlabs_api_key_here" "$(API_KEY_FILE)"; then \
			echo "$(GREEN)✅ API key is configured$(NC)"; \
		else \
			echo "$(RED)❌ API key file exists but appears to be template/empty$(NC)"; \
			echo "Please add your actual ElevenLabs API key to: $(API_KEY_FILE)"; \
			exit 1; \
		fi \
	else \
		echo "$(RED)❌ API key file not found: $(API_KEY_FILE)$(NC)"; \
		echo "Run 'make setup' first, then add your API key"; \
		exit 1; \
	fi

# Run all tests
test: test-unit test-integration

# Run unit tests only
test-unit:
	@echo "$(GREEN)Running unit tests...$(NC)"
	@$(EMACS) -batch -l $(PACKAGE_FILE) -l $(TEST_FILE) \
		--eval "(ert-run-tests-batch-and-exit \"^test-elevenlabs-tts-[^i]\")" \
		2>&1 | sed 's/^/  /' || true
	@echo "$(GREEN)Unit tests completed!$(NC)"

# Run integration tests (requires API key)
test-integration: check-api-key
	@echo "$(GREEN)Running integration tests...$(NC)"
	@echo "$(YELLOW)This will make actual API calls to ElevenLabs$(NC)"
	@$(EMACS) -batch -l $(PACKAGE_FILE) -l $(TEST_FILE) \
		--eval "(ert-run-tests-batch-and-exit \"test-elevenlabs-tts-integration\")" \
		2>&1 | sed 's/^/  /' || true
	@echo "$(GREEN)Integration tests completed!$(NC)"

# Manual test - generate a test audio file
test-manual: check-api-key clean
	@echo "$(GREEN)Running manual TTS test...$(NC)"
	@echo "$(YELLOW)Generating audio with Rachel voice...$(NC)"
	@$(EMACS) -batch -l $(PACKAGE_FILE) \
		--eval '(let ((voice-id (elevenlabs-tts--get-voice-id "Rachel")) (test-phrase "Hello, this is a test of the ElevenLabs text to speech system. Testing one, two, three.") (output-file "$(TEST_OUTPUT)")) (if (elevenlabs-tts--make-api-request-sync voice-id test-phrase output-file) (progn (message "✅ Audio successfully generated: %s" output-file) (message "File size: %d bytes" (nth 7 (file-attributes output-file)))) (message "❌ Audio generation failed")))' \
		2>&1 | grep -E "(✅|❌|Audio|File size|Making request)" || true
	@if [ -f "$(TEST_OUTPUT)" ]; then \
		echo "$(GREEN)✅ Test audio file created: $(TEST_OUTPUT)$(NC)"; \
		echo "$(YELLOW)You can play this file to verify TTS functionality$(NC)"; \
	else \
		echo "$(RED)❌ Test audio file was not created$(NC)"; \
	fi

# Lint/check the Emacs Lisp code
lint:
	@echo "$(GREEN)Linting Emacs Lisp code...$(NC)"
	@$(EMACS) -batch -l $(PACKAGE_FILE) \
		--eval "(progn \
			(setq byte-compile-error-on-warn t) \
			(byte-compile-file \"$(PACKAGE_FILE)\"))" \
		2>&1 | sed 's/^/  /' || true
	@$(EMACS) -batch -l $(TEST_FILE) \
		--eval "(progn \
			(setq byte-compile-error-on-warn t) \
			(byte-compile-file \"$(TEST_FILE)\"))" \
		2>&1 | sed 's/^/  /' || true
	@rm -f *.elc
	@echo "$(GREEN)Linting completed!$(NC)"

# Clean up test files and build artifacts
clean:
	@echo "$(GREEN)Cleaning up test files...$(NC)"
	@rm -f $(TEST_OUTPUT)
	@rm -f test-tts-*.mp3
	@rm -f *.elc
	@echo "$(GREEN)Cleanup completed!$(NC)"

# Development targets
dev-install: install
	@echo "$(GREEN)Development installation complete!$(NC)"
	@echo "Run 'make test' to verify everything works"

# Uninstall the package
uninstall:
	@echo "$(GREEN)Uninstalling ElevenLabs TTS package...$(NC)"
	@rm -rf $(INSTALL_DIR)
	@echo "$(YELLOW)Config directory preserved: $(CONFIG_DIR)$(NC)"
	@echo "$(GREEN)Package uninstalled!$(NC)"

# Show package information
info:
	@echo "$(GREEN)ElevenLabs TTS Package Information$(NC)"
	@echo "Package file: $(PACKAGE_FILE)"
	@echo "Test file: $(TEST_FILE)"
	@echo "Install directory: $(INSTALL_DIR)"
	@echo "Config directory: $(CONFIG_DIR)"
	@echo "API key file: $(API_KEY_FILE)"
	@echo ""
	@echo "$(YELLOW)Status:$(NC)"
	@if [ -d "$(INSTALL_DIR)" ]; then \
		echo "✅ Package installed"; \
	else \
		echo "❌ Package not installed"; \
	fi
	@if [ -f "$(API_KEY_FILE)" ]; then \
		echo "✅ API key file exists"; \
	else \
		echo "❌ API key file missing"; \
	fi
