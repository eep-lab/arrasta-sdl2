from PIL import Image, ImageDraw, ImageFont

# Create a blank image with a white background
width, height = 500, 200
image = Image.new('RGB', (width, height), 'white')

# Create a drawing object
draw = ImageDraw.Draw(image)

# Specify the font file and font size
font_file = 'font/Raleway-Regular.ttf'
font_size = 40

# Load the font and set the size
font = ImageFont.truetype(font_file, font_size)

# Specify the text content and position
text = "Hello, World!"
text_position = (50, 50)

# Set the text color
text_color = (0, 0, 0)  # Black color

# Draw the text on the image
draw.text(text_position, text, font=font, fill=text_color)

# Save the image as a PNG file
image.save('output.png', 'PNG')
