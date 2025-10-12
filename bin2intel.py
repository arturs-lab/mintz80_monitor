from intelhex import IntelHex

def bin_to_intelhex(input_bin_file, output_hex_file, offset=0):
    """
    Converts a binary file to Intel HEX format.

    Args:
        input_bin_file (str): Path to the input binary file.
        output_hex_file (str): Path to the output Intel HEX file.
        offset (int): Starting address offset for the data in the HEX file.
    """
    try:
        ih = IntelHex()
        ih.loadbin(input_bin_file, offset=offset)
        ih.write_hex_file(output_hex_file)
        print(f"Successfully converted '{input_bin_file}' to '{output_hex_file}' with offset {offset}.")
    except FileNotFoundError:
        print(f"Error: Input binary file '{input_bin_file}' not found.")
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    # Example usage:
    # Create a dummy binary file for demonstration
    # with open("BASIC-4k-stripped.bin", "wb") as f:
    #    f.write(b'\x01\x02\x03\x04\x05\x06\x07\x08')

    # Convert the dummy binary file to Intel HEX
    bin_to_intelhex("BASIC-4k-stripped.bin", "output.hex", offset=0x0000)
