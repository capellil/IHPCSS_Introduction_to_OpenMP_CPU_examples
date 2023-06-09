CC = gfortran
CFLAGS += -O3 -fopenmp
SRC_DIR=src
BIN_DIR=bin

default: all

all: create_directory \
	 $(BIN_DIR)/preamble

create_directory:
	@mkdir -p $(BIN_DIR)

$(BIN_DIR)/preamble: $(SRC_DIR)/preamble.F08
	$(CC) -o $@ $< $(CFLAGS)
