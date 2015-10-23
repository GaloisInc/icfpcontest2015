/* @flow */

import { List, Map, Record } from 'immutable'
import { compose, getter }   from 'lens'
import { field, index }      from 'lens/immutable'

import type { Getter, Lens_, Traversal_ } from 'lens'

export type AppState = Record<{
  apiToken: ?string,
  teams: ?List<Team>,
  user: ?Object,
  view: View,
  routeParams: Object,
  implementations: Map<TeamId,Filename>
}>

export type View = 'login' | 'editteam' | 'newteam' | 'teams'
export type Email = string
export type TeamId = number
export type Filename = string

export type Team = {
  id?:     TeamId,
  name:    string,
  members: Email[],
}

var AppStateRecord = Record({
  apiToken: null,
  teams: null,
  user: null,
  view: 'login',
  routeParams: {},
  implementations: Map(),
})

var initialState: AppState = new AppStateRecord()

var apiToken: Lens_<AppState,?string> = field('apiToken')
var teams: Lens_<AppState,?List<Team>> = field('teams')
var user: Lens_<AppState,?Object> = field('user')
var view: Lens_<AppState,View> = field('view')
var routeParams: Lens_<AppState,Object> = field('routeParams')
var implementations: Lens_<AppState,Map<TeamId,Filename>> = field('implementations')

function implementation(teamId: TeamId): Traversal_<AppState,Filename> {
  return compose(field('implementations'), index(teamId))
}

var team: Getter<AppState,?Team> = getter(state => {
  var teams = state.teams || List()
  var teamId = state.routeParams.teamId
  return teamId ? teams.find(team => team.id === parseInt(teamId, 10)) : null
})

export {
  initialState,
  apiToken,
  teams,
  team,
  user,
  view,
  routeParams,
  implementation,
  implementations,
}
