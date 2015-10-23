/* @flow */

import type { Team } from './state'

// Tells Flow that `fetch` is in global scope.
// see https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
declare var fetch: Function;

export {
  createSession,
  fetchTeams,
  postNewTeam,
  putTeam,
  postImplementation,
}

function createSession(assertion: string): Promise<{ apiToken: string }> {
  return fetch('/session',
    acceptJson(jsonBody({ assertion }, postRequest()))
  )
  .then(response => response.json())
}

function fetchTeams(token: string): Promise<Team[]> {
  return fetch('/teams',
    withToken(token, acceptJson(getRequest()))
  )
  .then(response => response.json())
}

function postNewTeam(token: string, team: Team): Promise<Team[]> {
  return fetch('/teams',
    withToken(token, jsonBody(team, postRequest()))
  )
  .then(_ => fetchTeams(token))
}

function putTeam(token: string, team: Team): Promise<void> {
  return fetch(`/teams/${team.id}`,
    withToken(token, jsonBody(team, { method: 'PUT', headers: {} }))
  )
}

function postImplementation(token: string, teamId: number, file: File): Promise<void> {
  return fetch(`/teams/${teamId}/implementation`, withToken(token, {
    method: 'POST',
    body: file,
    headers: {
      'Content-Type': 'application/octet-stream',
      'Content-Disposition': `inline; filename="${file.name}"`
    }
  }))
}

function acceptJson(opts: Object): Object {
  opts.headers['Accept'] = 'application/json'
  return opts
}

function jsonBody(data: Object, opts: Object): Object {
  var content = JSON.stringify(data)
  opts.headers['Content-Type'] = 'application/json'
  opts.headers['Content-Length'] = String(content.length)
  opts.body = content
  return opts
}

function withToken(apiToken: string, opts: Object): Object {
  var creds = `:${apiToken}`
  opts.headers['Authorization'] = `Basic ${btoa(creds)}`
  return opts
}

function postRequest(): Object {
  return {
    method: 'POST',
    headers: {}
  }
}

function getRequest(): Object {
  return {
    method: 'GET',
    headers: {}
  }
}

