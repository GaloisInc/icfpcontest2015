ICFP Submissions
================

Basic workflow during development:

With running server, navigate to `http://$ICFP_HOST/`.
Click link to log in with Persona.
Make a note of the API token that is displayed.

Post some solutions. E.g.:

    curl --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
        -d '[{"problemId":1,"seed":1,"solution":"upupdowndownleftrightleftrightba"},  \
             {"problemId":1,"seed":2,"solution":"upupdowndownleftrightleftrightba"}]' \
        http://$ICFP_HOST/teams/$TEAM_ID/solutions

Getting the ID of teams and problems is not implemented yet -
for now use `1` for the team ID and for the problem ID.

Fetch solutions:

    curl --user :$API_TOKEN -X GET http://$ICFP_HOST/teams/$TEAM_ID/solutions
