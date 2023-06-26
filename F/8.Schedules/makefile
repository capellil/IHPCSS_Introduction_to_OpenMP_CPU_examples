CC = gfortran
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/schedules

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/schedules: $(SRC_DIR)/schedules.f08
	$(CC) -o $@ $< $(CFLAGS)
