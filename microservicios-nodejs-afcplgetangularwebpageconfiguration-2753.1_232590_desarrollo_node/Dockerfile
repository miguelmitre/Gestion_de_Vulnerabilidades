#source nodejs
FROM registry-dev.aforecoppel.com:9443/library/node:20-alpine

# Create work directory
WORKDIR /usr/src/app

# Install runtime dependencies
##RUN npm install yarn -g

# Copy app source to work directory
COPY . /usr/src/app

# Install app dependencies
#RUN npm install --proxy=http://proxy.coppel.com:8080

# Build and run the app
CMD npm start
