CONTIKI_PROJECT = load_manager
all: $(CONTIKI_PROJECT)
# Do not try to build on Sky because of code size limitation
PLATFORMS_EXCLUDE = sky z1

# Include CoAP resources
MODULES_REL += ../Utility/RandomNumberGenerator
MODULES_REL += ../Utility/Timestamp
MODULES_REL += ../Utility/JSONSenML
MODULES_REL += ../Utility/Leds
MODULES_REL+=./resources
MODULES_REL+=/home/iot_ubuntu_intel/.local/lib/python3.10/site-packages/emlearn

TARGET_LIBFILES += -lm

INC+= /home/iot_ubuntu_intel/.local/lib/python3.10/site-packages/emlearn

CONTIKI=../../../..

ifeq ($(TARGET), cooja)
CFLAGS += -DCOOJA
endif
CFLAGS+=-Wno-error=unused-variable
CFLAGS+=-Wno-error=unused-function
CFLAGS+=-Wno-error=unused-but-set-variable
# Include the CoAP implementation
include $(CONTIKI)/Makefile.dir-variables
MODULES += $(CONTIKI_NG_APP_LAYER_DIR)/coap

include $(CONTIKI)/Makefile.include