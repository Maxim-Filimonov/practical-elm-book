const Lokka = require("lokka").Lokka;
const Transport = require("lokka-transport-http").Transport;

const client = new Lokka({
  transport: new Transport(
    "https://practical-elm.herokuapp.com/v1alpha1/graphql",
  ),
});

exports.handler = async (event, context) => {
  try {
    const result = await client.query(`
    {
      plans: SavedPlan {
        planVersions: PlanVersions {
          planText
          createdAt
          version
        }
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
