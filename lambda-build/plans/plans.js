const Lokka = require("lokka").Lokka;
const Transport = require("lokka-transport-http").Transport;

const client = new Lokka({
  transport: new Transport(
    "https://practical-elm.herokuapp.com/v1alpha1/graphql",
  ),
});

exports.handler = async (event, context) => {
  try {
    console.dir(event.headers);
    const result = await client.query(`
    {
      plans: SavedPlan {
        planVersions: PlanVersions {
          planText
          createdAt
          version
        }
        id
        name
      }
    }
    
    `);
    return {
      statusCode: 200,
      body: JSON.stringify(result),
    };
  } catch (err) {
    return { statusCode: 500, body: err.toString() };
  }
};
