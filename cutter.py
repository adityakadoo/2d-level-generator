from PIL import Image
import os
import sys

def cut_and_save_vertical(image_path, num_pieces):
    # Open the image
    img = Image.open(image_path)
    
    # Get the dimensions of the image
    width, height = img.size
    
    # Calculate the width of each piece
    piece_width = width // num_pieces
    
    # Create a directory to save the pieces
    output_dir = os.path.splitext(image_path)[0] + "_pieces"
    os.makedirs(output_dir, exist_ok=True)
    
    # Cut the image vertically and save each piece
    for i in range(num_pieces):
        left = i * piece_width
        upper = 0
        right = (i + 1) * piece_width
        lower = height
        piece = img.crop((left, upper, right, lower))
        piece.save(os.path.join(output_dir, f"piece_{i}.png"))
    
    print(f"{num_pieces} pieces saved in '{output_dir}'")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script.py <image_path> <num_pieces>")
        sys.exit(1)
    
    image_path = sys.argv[1]
    num_pieces = int(sys.argv[2])

    cut_and_save_vertical(image_path, num_pieces)
