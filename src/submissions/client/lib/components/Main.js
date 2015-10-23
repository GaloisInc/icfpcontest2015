/* @flow */

import * as Sunshine    from 'sunshine/react'
import React            from 'react'
import {
  Button,
  ButtonInput,
  ButtonToolbar,
  Input,
  Nav,
  Navbar,
  NavItem,
  PageHeader,
  Panel
} from 'react-bootstrap'
import { get, lookup }  from 'lens'
import * as State       from '../state'
import * as Ev          from '../event'

import type { List } from 'immutable'
import type { AppState, View, Team } from '../state'

type Props = {
  app: Sunshine.App<AppState>
}

type ComponentState = {
  apiToken: ?string,
  view: View,
  team: ?Team,
  teams: ?List<Team>,
}

export default class Main extends Sunshine.Component<{},Props,ComponentState> {
  getState(state: AppState): ComponentState {
    return {
      apiToken: get(State.apiToken, state),
      view: get(State.view, state),
      team: get(State.team, state),
      teams: get(State.teams, state),
    }
  }

  render(): React.Element {
    var content
    if (this.state.view === 'login' || !this.state.apiToken) {
      content = <Login/>
    }
    else if (!this.state.teams) {
      content = <Loading/>
    }
    else if (this.state.view === 'newteam') {
      content = <NewTeam/>
    }
    else if (this.state.view === 'editteam' && this.state.team) {
      content = <EditTeam team={this.state.team} />
    }
    else if (this.state.view === 'teams') {
      if (this.state.teams.size < 1) {
        content = <NewTeam/>
      }
      else {
        content = <TeamsComponent teams={this.state.teams} apiToken={this.state.apiToken} />
      }
    }

    if (this.state.apiToken) {
      return (
        <div>
          <Navbar brand='ICFP 2015'>
            <Nav right eventKey={0}>
              <NavItem eventKey={1} href='#/teams/new'>New team</NavItem>
              <NavItem eventKey={2} href='#/logout'>Log out</NavItem>
            </Nav>
          </Navbar>
          {content}
        </div>
      )
    }
    else {
      return (
        <div>
          <Navbar brand='ICFP 2015'>
          </Navbar>
          {content}
        </div>
      )
    }
  }
}

class Loading extends Sunshine.Component<{},{},{}> {
  render(): React.Element {
    return (
      <div className='container'>
        <p>Loading...</p>
      </div>
    )
  }
}

class Login extends Sunshine.Component<{},{},{}> {
  render(): React.Element {
    return (
      <div className='container'>
        <PageHeader>
          Please log in
        </PageHeader>
        <p>
        <Button bsStyle='primary' bsSize='large' onClick={this.login.bind(this)}>
          Log in
        </Button>
        </p>
      </div>
    )
  }

  login() {
    this.emit(new Ev.LoginRequest())
  }
}

class NewTeam extends Sunshine.Component<{},{},{}> {
  render(): React.Element {
    return (
      <div className='container'>
        <PageHeader>
          Name your team
        </PageHeader>
        <form className='form-horizontal' onSubmit={this.onSubmit.bind(this)}>
          <Input type='text' label='Team name' name='name' ref='name' />
          <Input type='textarea' label='Add team members, one email address per line' name='emails' ref='emails' />
          <ButtonInput type='submit' value='Create team' />
        </form>
      </div>
    )
  }

  onSubmit(event: Event) {
    event.preventDefault()
    var name = this.refs.name.getValue()
    var text = this.refs.emails.getValue()
    var emails = parseEmails(text)
    this.emit(new Ev.NewTeam({ name, members: emails }))
  }
}

class EditTeam extends Sunshine.Component<{},{ team: Team },{}> {
  render(): React.Element {
    var name = this.props.team.name
    return (
      <div className='container'>
        <PageHeader>
          Edit team
        </PageHeader>
        <form className='form-horizontal' onSubmit={this.onSubmit.bind(this)}>
          <Input type='text' label='Team name' name='name' ref='name' />
          <Input type='textarea' label='Add team members, one email address per line' name='emails' ref='emails' />
          <ButtonToolbar>
            <Button type='submit'>Update team</Button>
            <Button bsStyle='link' onClick={goBack}>cancel</Button>
          </ButtonToolbar>
        </form>
      </div>
    )
  }

  componentDidMount() {
    var input = React.findDOMNode(this.refs.name.refs.input)
    input.value = this.props.team.name
  }

  onSubmit(event: Event) {
    event.preventDefault()
    var id = this.props.team.id
    var name = this.refs.name.getValue()
    var text = this.refs.emails.getValue()
    var members = parseEmails(text)
    this.emit(new Ev.UpdateTeam({ id, name, members }))
  }
}

function parseEmails(text: string): string[] {
  var re = /\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b/i
  var lines = text.split("\n")
  return lines.map(l => (l.match(re) || [])[0]).filter(addr => !!addr)
}

function goBack(event: Event) {
  event.preventDefault()
  window.history.back()
}

type TeamsProps = {
  apiToken: string,
  teams: List<Team>
}

class TeamsComponent extends Sunshine.Component<{},TeamsProps,{}> {
  render(): React.Element {
    var teams = this.props.teams
    var title = teams.size === 1 ? "Your team" : "Your teams"
    return (
      <div className='container'>
        <PageHeader>{title}</PageHeader>
        {teams.map((team, i) => (
          <div>
            <TeamComponent team={team} key={team.id} apiToken={this.props.apiToken} />
            {(i < teams.size - 1) ? <hr/> : ''}
          </div>
        )).toJS()}
      </div>
    )
  }
}

type TeamProps = {
  apiToken: string,
  team: Team,
}

class TeamComponent extends Sunshine.Component<{},TeamProps,{ showToken: boolean }> {
  constructor(props, context) {
    super(props, context)
    this.state = { showToken: false }
  }

  render(): React.Element {
    var { id, name, members } = this.props.team

    var toggle = event => {
      event.preventDefault()
      this.setState({ showToken: !this.state.showToken })
    }

    var tokenPanel
    if (this.state.showToken) {
      tokenPanel =
        (<span><strong>{this.props.apiToken}</strong> (<a href='#' onClick={toggle}>click to hide</a>)</span>)
    }
    else {
      tokenPanel =
        (<span>&hellip; (<a href='#' onClick={toggle}>click to show</a>)</span>)
    }

    return (
      <div>
        <Navbar brand={name}>
          <Nav>
            <NavItem eventKey={0} href={`#/teams/${id}/edit`}>
              change name or add members
            </NavItem>
          </Nav>
        </Navbar>
        <p>
          members: <ul>{members.map(m => <li key={m}>{m}</li>)}</ul>
        </p>
        <p>Submit solutions to: <strong>{`${window.location.origin}/teams/${id}/solutions`}</strong></p>
        <Panel>
          <p>Your API token is: {tokenPanel}</p>
        </Panel>
        <Panel>
          <UploadImplementation {...this.props} />
        </Panel>
      </div>
    )
  }
}

type UploadState = {
  uploadedFilename: ?string
}

class UploadImplementation extends Sunshine.Component<{},TeamProps,UploadState> {
  getState(state: AppState): UploadState {
    var teamId = this.props.team.id
    return {
      uploadedFilename: lookup(State.implementation(teamId), state)
    }
  }

  render(): React.Element {
    var filename = this.state.uploadedFilename
    if (filename) {
      return this.renderFilename(filename)
    }
    else {
      return this.renderForm()
    }
  }

  renderForm(): React.Element {
    return (
      <form className='form-inline' onSubmit={this.onSubmitImplementation.bind(this)}>
        <Input type='file' label='Upload a code implementation' name='implementation' ref='implementation' />
        <ButtonInput type='submit' value='upload' />
      </form>
    )
  }

  renderFilename(filename: string): React.Element {
    return (
      <p>
        Uploaded: {filename} (<a href="#" onClick={this.replace.bind(this)}>replace</a>)
      </p>
    )
  }

  onSubmitImplementation(event: Event) {
    event.preventDefault()
    var input = React.findDOMNode(this.refs.implementation.refs.input)
    var file  = input.files[0]
    this.emit(new Ev.UploadImplementation(this.props.team.id, file))
  }

  replace(event: Event) {
    event.preventDefault()
    this.emit(new Ev.ReplaceImplementation(this.props.team.id))
  }
}
