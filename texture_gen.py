from PIL import Image
import os, sys
import xml.etree.ElementTree as ET
from pprint import pprint

def join_images(directory, output_path, file_info, scale_factor=1):
    images = {}

    # Iterate through the directory to find PNG files
    for filename in file_info.keys():
        images[file_info[filename]['idx']//8] = Image.open(os.path.join(directory, filename+".png"))

    # Get dimensions of the first image
    width, height = images[0].size

    # Create a new image with the width of all input images combined and the height of the first image
    new_image = Image.new('RGB', (width * len(images.keys()) * scale_factor, height * scale_factor))

    # Paste each image side by side
    for i, img in images.items():
        # new_width = int(width * scale_factor)
        # new_height = int(height * scale_factor)
        # scaled_img = img.resize((new_width, new_height), Image.LANCZOS)
        new_image.paste(img, (i * width * scale_factor, 0))

    # Save the resulting image
    new_image.save(output_path)

def rot(c):
    if c == 'X':
        s = 1
    elif c == 'I' or c == '/':
        s = 2
    else:
        s = 4
    return lambda r: 4*(r//4) + (((r%4) +1) %s)

def flip(c):
    if c == 'I' or c == 'X':
        return lambda r: r
    if c == '/':
        return lambda r: 2*(r//2) + (1 - r%2)
    if c == 'T':
        return lambda r: r if r%2 == 1 else 4*(r//4) + 2*(1 - (r%4)//2)
    if c == 'L':
        return lambda r: 4*(r//4) + (3 - r%4)
    if c == 'F':
        return lambda r: 8*(r//8) + ((r%8)+4)%8

filename = sys.argv[1]


xml_file = f"resources/tilesets/{filename}/constraints.xml"
xml_data = ET.parse(xml_file)

t_info = {}
t_edges = {}
cnt = 0
sym = {
    'I': 2,
    'X': 1,
    'T': 4,
    'L': 4,
    '/': 2,
    'F': 8
}
for tile in xml_data.find('tiles').findall('tile'):
    t_info[tile.get('name')] = {
        'sym': tile.get('symmetry'),
        'weight': tile.get('weight'),
        'idx': cnt
    }
    for i in range(sym[tile.get('symmetry')]):
        t_edges[cnt+i] = {
            'l': set(),
            'r': set(),
            'u': set(),
            'd': set()
        }
    cnt += 8

input_directory = f"resources/tilesets/{filename}/parts"
output_image_path = f"resources/tilesets/{filename}/whole.png"
join_images(input_directory, output_image_path, t_info)

d = ['r', 'u', 'l', 'd']
for edge in xml_data.find('neighbors').findall('neighbor'):
    l = edge.get('left').split(' ')
    r = edge.get('right').split(' ')
    l_tile = l[0]
    r_tile = r[0]
    l_rot = 0 if len(l) == 1 else int(l[1])
    r_rot = 0 if len(r) == 1 else int(r[1])
    l_idx = t_info[l_tile]['idx']+l_rot
    r_idx = t_info[r_tile]['idx']+r_rot
    l_f = rot(t_info[l_tile]['sym'])
    r_f = rot(t_info[r_tile]['sym'])

    for i in range(4):
        t_edges[l_idx][d[i]].add(r_idx)
        t_edges[r_idx][d[(i+2)%4]].add(l_idx)
        l_idx = l_f(l_idx)
        r_idx = r_f(r_idx)
    
    l_idx = flip(t_info[l_tile]['sym'])(l_idx)
    r_idx = flip(t_info[r_tile]['sym'])(r_idx)
    for i in range(4):
        t_edges[l_idx][d[i]].add(r_idx)
        t_edges[r_idx][d[(i+2)%4]].add(l_idx)
        l_idx = l_f(l_idx)
        r_idx = r_f(r_idx)


# pprint(t_edges)

hs_outfile = f"src/TS{filename.title()}.hs"
with open(hs_outfile, '+w') as f:
    f.writelines([
        f"module TS{filename.title()} where\n",
        "\n",
        "import WaveFuncCollapse (Tile)\n",
        "\n",
        "textureImg :: String\n",
        f"textureImg = \"resources/tilesets/{filename}/whole.png\"\n",
        "\n",
        "tileIdx :: [Int]\n",
        f"tileIdx = {list(t_edges.keys())}\n",
        "\n",
        "nImages :: Int\n",
        f"nImages = {len(t_info.keys())}\n",
        "\n",
        "tileInfo :: Tile -> Int\n",
        "tileInfo t\n"
    ])
    for idx, tilename in enumerate(t_info.keys()):
        f.write(f"  | q=={idx} = {t_info[tilename]['weight']}\n")
    f.writelines([
        "  where\n",
        "    q = t `div` 8\n",
        "\n",
        "tileNeighbours :: Tile -> [[Int]]\n"
    ])
    for key, val in t_edges.items():
        f.writelines([
            f"tileNeighbours {key} = [\n",
            f"    {list(val['u'])},\n",
            f"    {list(val['l'])},\n",
            f"    {list(val['r'])},\n",
            f"    {list(val['d'])}\n",
            "  ]\n"
        ])
