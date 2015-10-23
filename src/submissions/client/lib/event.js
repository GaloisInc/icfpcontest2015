/* @flow */

import * as Sunshine      from 'sunshine'
import { get, over, set } from 'lens'
import { List }           from 'immutable'
import * as State         from './state'
import { createSession, fetchTeams, postNewTeam, putTeam, postImplementation } from './api'

import type { AppState, Team, View } from './state'

/* routing events */

class LoginRequest {}
class LogoutRequest {}

class NewSession {
  apiToken: string;
  constructor(apiToken: string) { this.apiToken = apiToken }
}

class DestroySession {}

class Route {
  view: View;
  params: Object;
  constructor(view: View, params?: Object) {
    this.view = view
    this.params = params || {}
  }
}

class NewTeam {
  team: Team;
  constructor(team: Team) {
    this.team = team
  }
}

class UpdateTeam {
  team: Team;
  constructor(team: Team) {
    this.team = team
  }
}

class Teams {
  teams: Team[];
  constructor(teams: Team[]) {
    this.teams = teams
  }
}

class UploadImplementation {
  teamId: number;
  file: File;
  constructor(teamId: number, file: File) {
    this.teamId = teamId
    this.file = file
  }
}

class Implementation {
  teamId: number;
  filename: string;
  constructor(teamId: number, filename: string) {
    this.teamId = teamId
    this.filename = filename
  }
}

class ReplaceImplementation {
  teamId: number;
  constructor(teamId: number) { this.teamId = teamId }
}

class GenericError {
  message: string;
  constructor(message: string) { this.message = message }
}

function init(initialState: AppState, app: Sunshine.App<AppState>, router: Object) {
  app.on(LoginRequest, (_, __) => {
    navigator.id.request()  // Initiates BrowserID authentication
  })

  app.on(LogoutRequest, (_, __) => {
    navigator.id.logout()
    router.location.go('/login')
  })

  app.on(NewSession, (state, { apiToken }) => {
    fetchTeams(apiToken).then(teams => {
      app.emit(new Teams(teams))
    })
    return set(State.apiToken, apiToken, state)
  })

  app.on(DestroySession, (state, __) => {
    router.location.go('/login')
    return set(State.apiToken, null, state)
  })

  app.on(Route, (state, { view, params }) => {
    var token = get(State.apiToken, state)
    if ((view === 'teams' || view === 'editteam') && token) {
      fetchTeams(token).then(teams => {
        app.emit(new Teams(teams))
      })
    }
    return set(State.routeParams, params,
           set(State.view, view,
           state))
  })

  app.on(NewTeam, (state, { team }) => {
    withToken(state, (token) => {
      postNewTeam(token, team)
      .then(
        // on success
        teams => {
          router.location.go('/teams')
          app.emit(new Teams(teams))
        },
        // on error
        () => {
          app.emit(new GenericError('An error occurred while creating a team'))
        }
      )
    })
  })

  app.on(UpdateTeam, (state, { team }) => {
    withToken(state, token => {
      putTeam(token, team)
      .then(
        // on success
        () => {
          router.location.go('/teams')
        },
        // on error
        () => {
          app.emit(new GenericError('An error occurred while updating team'))
        }
      )
    })
  })

  app.on(Teams, (state, { teams }) => {
    return set(State.teams, List(teams), state)
  })

  app.on(UploadImplementation, (state, { teamId, file }) => {
    withToken(state, token => {
      postImplementation(token, teamId, file)
      .then(_ => app.emit(new Implementation(teamId, file.name)))
    })
  })

  app.on(Implementation, (state, { teamId, filename }) => {
    return over(State.implementations, m => m.set(teamId, filename), state)
  })

  app.on(ReplaceImplementation, (state, { teamId }) => {
    return over(State.implementations, m => m.remove(teamId), state)
  })

  app.on(GenericError, (state, { message }) => {
    console.error(message)
  })

  // Handles BrowserID events
  navigator.id.watch({
    loggedInUser: get(State.user, initialState),
    onlogin(assertion) {
      if (router.location.get() === '/login') {
        router.location.go('/teams')
      }
      createSession(assertion)
      .then(
        // on success
        session => {
          app.emit(new NewSession(session.apiToken))
        },
        // on failure
        () => {
          app.emit(new GenericError('An error occurred during authentication.'))
          router.location.go('/login')
        }
      )
    },
    onlogout() {
      app.emit(new DestroySession())
    }
  })

  function withToken<T>(state: AppState, f: (token: string) => T): ?T {
    var token = get(State.apiToken, state)
    if (token) {
      return f(token)
    } else {
      app.emit(new GenericError('No API token found'))
    }
  }
}

export {
  init,
  LoginRequest,
  LogoutRequest,
  NewTeam,
  UpdateTeam,
  Route,
  UploadImplementation,
  ReplaceImplementation,
  GenericError,
}
