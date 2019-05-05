const querystring = require("querystring");
const crypto = require("crypto");

exports.handler = async (event, context) => {
  try {
    if (event.httpMethod === "OPTIONS") {
      const response = {
        statusCode: 200,
        headers: {
          "Access-Control-Allow-Origin": event.headers.origin || "*",
          "Access-Control-Allow-Credentials": "true",
          "Access-Control-Allow-Headers":
            "Origin, X-Requested-With, Content-Type, Accept",
        },
        body: "",
      };
      return response;
    }
    if (event.httpMethod !== "POST") {
      return { statusCode: 405, body: "Method Not Allowed" };
    }
    const { username, password } = JSON.parse(event.body);
    if (username === "ADMIN" && password === "MAGIC") {
      const id = crypto.randomBytes(16).toString("hex");
      return {
        statusCode: 200,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
        },
        body: JSON.stringify({ sessionId: id }),
      };
    } else {
      return {
        statusCode: 401,
        headers: {
          "Content-Type": "application/json",
          "Access-Control-Allow-Origin": "*",
        },
        body: JSON.stringify({}),
      };
    }
  } catch (err) {
    return { statusCode: 500, body: err.toString() };
  }
};
