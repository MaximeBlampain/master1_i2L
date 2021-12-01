const jwt = require('jsonwebtoken');
const { SECRET } = require('../config')
const { getRouteFromRequest } = require('../utils/helpers')

module.exports = verifyToken;

function verifyToken(req, res, next) {
  var route = getRouteFromRequest(req);
  console.info(`[route] '${route}' - token verification`);
    var token = "";
  if (req.headers && req.headers.token) token = req.headers.token;
  else if (req.params && req.params.token) token = req.params.token;
  else if (req.body && req.body.token) token = req.body.token;

  const tokenHaveCorrectProperty = (decodedToken) => {
    return (
      decodedToken.hasOwnProperty('sub')
      && decodedToken.hasOwnProperty('exp')
      && decodedToken.hasOwnProperty('iat')
      && decodedToken.hasOwnProperty('id')
    )
  }

  if(token === "") {
    console.error(`[route] '${route}' - ERROR - Can't find token in request`);
    return res.status(400).json({ message: "Can't find token in request" })
  } else {
    jwt.verify(token, SECRET, (err, decodedToken) => {
      if(err) {
        if(err.name === "TokenExpiredError") {
          console.error(`[route] '${route}' - ERROR - Token Expired`);
          return res.status(403).json({ type: 'error', code: 'TOKEN_EXPIRED' })
        } else if (err.name === "JsonWebTokenError" || !decodedToken) {
          console.error(`[route] '${route}' - ERROR - Bad Token`);
          return res.status(403).json({ type: 'error', code: 'BAD_TOKEN' })
        } else {
          console.error(`[route] '${route}' - ERROR - Unknown Error`);
          console.log("err === ", err)
          console.log("err.name === ", err.name)
          return res.status(400).json({ type: 'error',  code: 'UNKNOWN_ERROR' })
        }
      } else {
        if (!tokenHaveCorrectProperty(decodedToken)) {
          console.error(`[route] '${route}' - ERROR - Bad Token`);
          return res.status(403).json({ type: 'error', code: 'BAD_TOKEN' })
        } else {
          console.info(`[route] '${route}' - SUCCESS - Token Verified`);
          next();
        }
      }
    })
  }
};
