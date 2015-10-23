/* @flow */

import * as Sunshine    from 'sunshine/react'
import React            from 'react'
import makeRouter       from 'hash-brown-router'
import { initialState } from './state'
import * as Event       from './event'
import Main             from './components/Main'
import {}               from './polyfills'

var app = new Sunshine.App(initialState, () => {
  var router = makeRouter()
  Event.init(initialState, app, router)

  router.add('/login', () => {
    app.emit(new Event.Route('login'))
  })

  router.add('/logout', () => {
    app.emit(new Event.LogoutRequest())
  })

  router.add('/teams/new', () => {
    app.emit(new Event.Route('newteam'))
  })

  router.add('/teams/:teamId/edit', (params) => {
    app.emit(new Event.Route('editteam', params))
  })

  router.add('/teams', () => {
    app.emit(new Event.Route('teams'))
  })

  router.setDefault((path, params) => {
    app.emit(new Event.GenericError('no route handler for: '+ path))
  })

  router.evaluateCurrent('/teams')
})

React.render(<Main app={app} />, document.getElementById('app'))
