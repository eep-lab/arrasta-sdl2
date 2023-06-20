import textwrap
from PIL import Image, ImageDraw, ImageFont
import tkinter as tk

# Function to get monitor screen size
def get_screen_size():
    root = tk.Tk()
    screen_width = root.winfo_screenwidth()
    screen_height = root.winfo_screenheight()
    root.destroy()
    return screen_width, screen_height

# Function to calculate the maximum width and height for the image
def calculate_max_size(screen_width, screen_height, border):
    max_width = screen_width - border
    max_height = screen_height - border
    return max_width, max_height

# Function to create a wrapped text image
def create_text_image(text, font_file, font_size, max_width, max_height, text_color):
    # Create a blank image with a white background
    image = Image.new('RGB', (max_width, max_height), 'white')

    # Create a drawing object
    draw = ImageDraw.Draw(image)

    # Load the font and set the size
    font = ImageFont.truetype(font_file, font_size)

    # Wrap the text based on the maximum width
    wrapped_text = textwrap.wrap(text, width=int(max_width / (font_size * 0.6)))

    # Calculate the total height required for the wrapped text
    total_text_height = len(wrapped_text) * font_size

    # Calculate the starting y-position for the text
    y = int((max_height - total_text_height) / 2)

    # Draw the wrapped text on the image
    for line in wrapped_text:
        text_bbox = draw.textbbox((0, 0, max_width, max_height), line, font=font)
        text_width = text_bbox[2] - text_bbox[0]
        text_height = text_bbox[3] - text_bbox[1]
        x = int((max_width - text_width) / 2)
        draw.text((x, y), line, font=font, fill=text_color)
        y += font_size

    return image

# Main code
text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus sed libero vitae tortor rhoncus pellentesque ut sed ex."

# Set the font file, font size, and text color
font_file = 'font/Raleway-Regular.ttf'
font_size = 40
text_color = (0, 0, 0)  # Black color

# Set the border size (in pixels) around the image
border = 100

# Get the screen size
screen_width, screen_height = get_screen_size()

# Calculate the maximum width and height for the image
max_width, max_height = calculate_max_size(screen_width, screen_height, border)

# Create the text image
image = create_text_image(text, font_file, font_size, max_width, max_height, text_color)

# Save the image as a PNG file
image.save('output_wrap.png', 'PNG')