# ElevenLabs Voice Reference

This document provides a comprehensive reference of all available voices in the ElevenLabs TTS Emacs package.

**Last updated:** 2025-08-25  
**Total voices:** 22 (7 female, 14 male, 1 neutral)

## Voice Categories

### Female Voices (7)

| Name | Voice ID | Accent | Age | Description |
|------|----------|--------|-----|-------------|
| Rachel | `21m00Tcm4TlvDq8ikWAM` | American | Young | Clear, professional female voice |
| Sarah | `EXAVITQu4vr4xnSDxMaL` | American | Young | Natural, friendly female voice |
| Laura | `FGY2WhTYpPnrIDTdsKH5` | American | Young | Expressive, warm female voice |
| Alice | `Xb7hH8MSUJpSbSDYk0k2` | British | Middle-aged | Sophisticated British female voice |
| Jessica | `cgSgspJ2msm6clMCkdW9` | American | Young | Bright, energetic female voice |
| Lily | `pFZP5JQG7iQjIQuC4Bku` | British | Middle-aged | Elegant British female voice |
| Matilda | `XrExE9yKIg1WjnnlVkGX` | American | Middle-aged | Mature, authoritative female voice |

### Male Voices (14)

| Name | Voice ID | Accent | Age | Description |
|------|----------|--------|-----|-------------|
| Clyde | `2EiwWnXFnvU5JabPnv8n` | American | Middle-aged | Confident, professional male voice |
| Roger | `CwhRBWXzGAHq8TQ4Fs17` | American | Middle-aged | Mature, authoritative male voice |
| Thomas | `GBv7mTt0atIp3Br8iCZE` | American | Young | Clear, youthful male voice |
| Charlie | `IKne3meq5aSn9XLyUdCD` | Australian | Young | Distinctive Australian male voice |
| George | `JBFqnCBsd6RMkjVDRZzb` | British | Middle-aged | Classic British male voice |
| Callum | `N2lVS1w4EtoT3dr4eOWO` | - | Middle-aged | Neutral accent male voice |
| Harry | `SOYHLrjzK2X1ezoPC6cr` | American | Young | Energetic, friendly male voice |
| Liam | `TX3LPaxmHKxFdv7VOQHJ` | American | Young | Natural, conversational male voice |
| Will | `bIHbv24MWmeRgasZH58o` | American | Young | Bright, cheerful male voice |
| Eric | `cjVigY5qzO86Huf0OWal` | American | Middle-aged | Professional, steady male voice |
| Chris | `iP95p4xoKVk53GoZ742B` | American | Middle-aged | Reliable, articulate male voice |
| Brian | `nPczCjzI2devNBz1zQrb` | American | Middle-aged | Deep, resonant male voice |
| Daniel | `onwK4e9ZLuTAKqWW03F9` | British | Middle-aged | Refined British male voice |
| Bill | `pqHfZKP75CvOlQylNhV4` | American | Old | Distinguished, mature male voice |

### Neutral Voices (1)

| Name | Voice ID | Accent | Age | Description |
|------|----------|--------|-----|-------------|
| River | `SAz9YHcvj6GT2YYXdXww` | American | Middle-aged | Gender-neutral, versatile voice |

## Usage in Emacs Configuration

### Default Voice Lists

The package comes pre-configured with these voices organized by gender:

```elisp
;; Female voices (in default order)
(setq elevenlabs-tts-female-voices 
      '("Rachel" "Sarah" "Laura" "Alice" "Jessica" "Lily" "Matilda"))

;; Male voices (in default order) 
(setq elevenlabs-tts-male-voices 
      '("Clyde" "Roger" "Thomas" "Charlie" "George" "Callum" 
        "Harry" "Liam" "Will" "Eric" "Chris" "Brian" "Daniel" "Bill"))
```

### Customizing Voice Order

You can customize the voice lists to put your preferred voices first:

```elisp
;; Put your favorite female voices first
(setq elevenlabs-tts-female-voices 
      '("Alice" "Rachel" "Lily" "Sarah" "Laura" "Jessica" "Matilda"))

;; Put your favorite male voices first
(setq elevenlabs-tts-male-voices 
      '("George" "Daniel" "Clyde" "Brian" "Roger" "Thomas" "Charlie" 
        "Callum" "Harry" "Liam" "Will" "Eric" "Chris" "Bill"))
```

### Including River (Neutral Voice)

The neutral voice "River" is included in voice selection but not in the quick gender-based commands. It appears in the full voice selection list when using `elevenlabs-tts-speak-selection`.

## Voice Selection Commands

| Command | Description |
|---------|-------------|
| `elevenlabs-tts-speak-selection` | Interactive selection from all available voices |
| `elevenlabs-tts-speak-selection-quick 'female` | Uses first female voice (Rachel by default) |
| `elevenlabs-tts-speak-selection-quick 'male` | Uses first male voice (Clyde by default) |

## File Naming Examples

When generating audio files, the voice name is included in lowercase:

- `document-0001-rachel.mp3` (Rachel)
- `document-0002-clyde.mp3` (Clyde)  
- `document-0003-alice.mp3` (Alice)
- `document-0004-george.mp3` (George)
- `document-0005-river.mp3` (River)

## Voice Verification

To verify all voices are working correctly, you can run:

```bash
# Check all voice IDs against ElevenLabs API
./reverse_lookup.sh

# Run integration tests (requires API key and credits)
make test-integration
```

## Notes

- **Voice Availability**: These voices are confirmed available as of 2025-08-25
- **API Changes**: ElevenLabs may add, remove, or modify voices; check their API documentation for updates
- **Voice IDs**: Voice IDs are stable identifiers that won't change even if voice names are updated
- **Accent/Age Labels**: Some voices may have incomplete metadata (marked as "N/A" or empty)

## Voice Quality Guidelines

For best results:
- **Professional content**: Alice, George, Daniel, Roger, Clyde, Brian
- **Casual content**: Rachel, Sarah, Harry, Liam, Will
- **Young audience**: Thomas, Jessica, Laura, Harry, Will
- **International content**: Charlie (Australian), Alice/George/Daniel/Lily (British)
- **Gender-neutral**: River

## Troubleshooting

If a voice doesn't work:
1. Check that the voice name is spelled exactly as shown above
2. Verify the voice ID exists in `elevenlabs-tts-voices` configuration
3. Run `./reverse_lookup.sh` to test voice IDs against the API
4. Check ElevenLabs API documentation for any voice changes
