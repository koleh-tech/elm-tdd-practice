# Use the Manjaro base image
FROM manjarolinux/base:latest

# Update the package lists
RUN pacman --noconfirm -Syu

# Install necessary packages
RUN pacman --noconfirm -S npm make
RUN npm install --global elm elm-live@next elm-format elm-test 

# Set the working directory
WORKDIR /app

# Copy your application files to the container
COPY . /app

# Specify the default command to run when the container starts
CMD ["elm-test"]
