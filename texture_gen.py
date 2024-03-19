from PIL import Image
import os

def join_images(directory, output_path):
    images = []

    # Iterate through the directory to find PNG files
    for filename in sorted(os.listdir(directory)):
        if filename.endswith('.png'):
            images.append(Image.open(os.path.join(directory, filename)))

    # Get dimensions of the first image
    width, height = images[0].size

    # Create a new image with the width of all input images combined and the height of the first image
    new_image = Image.new('RGB', (width * len(images), height))

    # Paste each image side by side
    for i, img in enumerate(images):
        new_image.paste(img, (i * width, 0))

    # Save the resulting image
    new_image.save(output_path)

# Example usage:
input_directory = "resources/tilesets-Circuit"
output_image_path = "resources/tilesets-Circuit/TEXTURE.png"
join_images(input_directory, output_image_path)