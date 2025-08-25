#!/bin/bash

# Voice IDs to lookup - Current available voices as of 2025-08-25
voice_ids=(
    # Female voices
    "21m00Tcm4TlvDq8ikWAM"  # Rachel (American, young)
    "EXAVITQu4vr4xnSDxMaL"  # Sarah (American, young)
    "FGY2WhTYpPnrIDTdsKH5"  # Laura (American, young)
    "Xb7hH8MSUJpSbSDYk0k2"  # Alice (British, middle-aged)
    "cgSgspJ2msm6clMCkdW9"  # Jessica (American, young)
    "pFZP5JQG7iQjIQuC4Bku"  # Lily (British, middle-aged)
    "XrExE9yKIg1WjnnlVkGX"  # Matilda (American, middle-aged)
    # Male voices
    "2EiwWnXFnvU5JabPnv8n"  # Clyde (American, middle-aged)
    "CwhRBWXzGAHq8TQ4Fs17"  # Roger (American, middle-aged)
    "GBv7mTt0atIp3Br8iCZE"  # Thomas (American, young)
    "IKne3meq5aSn9XLyUdCD"  # Charlie (Australian, young)
    "JBFqnCBsd6RMkjVDRZzb"  # George (British, middle-aged)
    "N2lVS1w4EtoT3dr4eOWO"  # Callum (middle-aged)
    "SOYHLrjzK2X1ezoPC6cr"  # Harry (American, young)
    "TX3LPaxmHKxFdv7VOQHJ"  # Liam (American, young)
    "bIHbv24MWmeRgasZH58o"  # Will (American, young)
    "cjVigY5qzO86Huf0OWal"  # Eric (American, middle-aged)
    "iP95p4xoKVk53GoZ742B"  # Chris (American, middle-aged)
    "nPczCjzI2devNBz1zQrb"  # Brian (American, middle-aged)
    "onwK4e9ZLuTAKqWW03F9"  # Daniel (British, middle-aged)
    "pqHfZKP75CvOlQylNhV4"  # Bill (American, old)
    # Neutral voices
    "SAz9YHcvj6GT2YYXdXww"  # River (American, middle-aged)
)

API_KEY=$(cat ~/.config/emacs-tts/elevenlabs-api-key)

echo "=== ElevenLabs Voice ID Reverse Lookup ==="
echo ""

for voice_id in "${voice_ids[@]}"; do
    echo "Voice ID: $voice_id"
    response=$(curl -s -H "xi-api-key: $API_KEY" "https://api.elevenlabs.io/v1/voices/$voice_id")
    
    if echo "$response" | jq -e . >/dev/null 2>&1; then
        name=$(echo "$response" | jq -r '.name // "N/A"')
        gender=$(echo "$response" | jq -r '.labels.gender // "N/A"')
        accent=$(echo "$response" | jq -r '.labels.accent // "N/A"')
        age=$(echo "$response" | jq -r '.labels.age // "N/A"')
        description=$(echo "$response" | jq -r '.labels.description // "N/A"')
        
        echo "  Name: $name"
        echo "  Gender: $gender"
        echo "  Accent: $accent"
        echo "  Age: $age"
        echo "  Description: $description"
    else
        echo "  ERROR: $response"
    fi
    echo ""
done
