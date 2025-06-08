CONTIKI_PROJECT = DR_manager
all: $(CONTIKI_PROJECT)
# Do not try to build on Sky because of code size limitation
PLATFORMS_EXCLUDE = sky z1

# Include CoAP resources
MODULES_REL += ../Utility/RandomNumberGenerator
MODULES_REL += ../Utility/JSONSenML
MODULES_REL += ../Utility/Timestamp
MODULES_REL += ../Utility/Leds
MODULES_REL+=./resources

CONTIKI=../../../..

# Include the CoAP implementation
include $(CONTIKI)/Makefile.dir-variables
MODULES += $(CONTIKI_NG_APP_LAYER_DIR)/coap

ifeq ($(TARGET), cooja)
CFLAGS += -DCOOJA
endif
CFLAGS+=-Wno-error=unused-variable
CFLAGS+=-Wno-error=unused-function

CFLAGS += -DCOAP_OBSERVE_CLIENT=1
include $(CONTIKI)/Makefile.include