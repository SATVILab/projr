# Use the official Ubuntu image as the base
FROM rocker/verse:4.4

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Copy the `.devcontainer/renv` folder to the container
COPY .devcontainer/renv /usr/local/share/config-r/renv

# Set appropriate permissions
RUN chmod -R 755 /usr/local/share/config-r/renv
