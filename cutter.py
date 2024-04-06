from PIL import Image
import os
import sys

def cut_and_save_vertical(image_path, num_pieces_x, num_pieces_y):
    # Open the image
    img = Image.open(image_path)
    
    # Get the dimensions of the image
    width, height = img.size
    
    # Calculate the width of each piece
    piece_width = width // num_pieces_x
    piece_height = height // num_pieces_y
    
    # Create a directory to save the pieces
    output_dir = os.path.splitext(image_path)[0] + "_pieces"
    os.makedirs(output_dir, exist_ok=True)
    
    # Cut the image vertically and save each piece
    for i in range(num_pieces_x):
        for j in range(num_pieces_y):
            left = i * piece_width
            upper = j * piece_height
            right = (i + 1) * piece_width
            lower = (j + 1) * piece_height
            piece = img.crop((left, upper, right, lower))
            piece.save(os.path.join(output_dir, f"piece_{j}_{i}.png"))
    
    print(f"{num_pieces_x*num_pieces_y} pieces saved in '{output_dir}'")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py <image_path> <num_pieces_x> <num_pieces_y>")
        sys.exit(1)
    
    image_path = sys.argv[1]
    num_pieces_x = int(sys.argv[2])
    num_pieces_y = int(sys.argv[3])

    cut_and_save_vertical(image_path, num_pieces_x, num_pieces_y)
