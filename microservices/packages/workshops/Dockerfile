FROM node:10

WORKDIR /usr/app

# Install app dependencies
# A wildcard is used to ensure both package.json AND package-lock.json are copied
# where available (npm@5+)
COPY package*.json ./

RUN npm install --production

COPY dist ./dist

EXPOSE 8080

ENV PORT=8080
ENV MONGO_URI=mongodb://mongo:27017

CMD ["npm", "start"]
