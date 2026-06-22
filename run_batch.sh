#!/bin/bash
# အပြင်ဆုံး Project Folder ထဲ အရင်ဝင်မယ်
cd "$(dirname "$0")"

# COBOL တွေ တကယ်ရှိတဲ့ src folder ထဲကို ထပ်ဝင်မယ်
cd src

# Library Path သတ်မှတ်မယ်
export COB_LIBRARY_PATH=bin

# bin/BATCHRUN ကို မောင်းပြီး log ဖိုင်ကို အပြင်ဆုံးမှာ သွားထုတ်မယ်
./bin/BATCHRUN >> ../batch_result.log 2>&1