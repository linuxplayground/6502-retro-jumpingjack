AS=ca65
LD=ld65
TOP = .

SERIAL_DEVICE=/dev/tty.usbserial-A5XK3RJT

LOAD_TRIM = $(TOP)/loadtrim.py
INCLUDES = -I $(TOP)

ASFLAGS = $(INCLUDES) -g --feature labels_without_colons --cpu 65C02 --feature string_escapes
CFG = $(TOP)/apps.cfg

APPNAME = jj
LOAD_ADDR = 800

BUILD_DIR = build
SOURCES = main.s \
	  vdp.s

OBJS = $(addprefix $(BUILD_DIR)/, $(SOURCES:.s=.o))
all: clean $(BUILD_DIR)/$(APPNAME).com
clean:
	rm -fr $(BUILD_DIR)/*
$(BUILD_DIR)/%.o: %.s
	@mkdir -p $$(dirname $@)
	$(AS) $(ASFLAGS) -l $(BUILD_DIR)/$*.lst $< -o $@

$(BUILD_DIR)/$(APPNAME).raw: $(OBJS)
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG) $^ -o $@ -m $(BUILD_DIR)/$(APPNAME).map -Ln $(BUILD_DIR)/$(APPNAME).sym

$(BUILD_DIR)/$(APPNAME).com: $(BUILD_DIR)/$(APPNAME).raw
	$(LOAD_TRIM) $(BUILD_DIR)/$(APPNAME).raw $(BUILD_DIR)/$(APPNAME).com $(LOAD_ADDR)

minicom:
	minicom -D $(SERIAL_DEVICE)
