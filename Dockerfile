# Use the Manjaro base image
FROM ubuntu:latest


# Update the package lists
RUN apt-get update

# Install necessary packages
RUN apt-get -y install npm make
RUN npm install --global elm elm-live@next elm-format elm-test 

# Set the working directory
WORKDIR /app

# Copy your application files to the container
COPY . /app

# Specify the default command to run when the container starts
CMD ["elm-test"]
