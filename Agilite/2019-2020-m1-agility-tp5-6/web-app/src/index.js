import React from 'react';
import ReactDOM from 'react-dom';

import './index.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import "shards-ui/dist/css/shards.min.css"

import Icon from '@material-ui/core/Icon';
import { green, red } from '@material-ui/core/colors';
import { makeStyles } from '@material-ui/core/styles';

import Card from 'react-bootstrap/Card'
import CardDeck from 'react-bootstrap/CardDeck'
import Table from 'react-bootstrap/Table'
import Form from 'react-bootstrap/Form'
import Modal from 'react-bootstrap/Modal'


import {
    Navbar,
    NavbarBrand,
    Nav,
    NavItem,
    NavLink,
    Collapse,
    Button,
    Progress,
} from "shards-react";

const serveurURI = 'http://127.0.0.1:4567'

/**
 * usefull icon functions
 * @type {(props?: any) => ClassNameMap<"root">}
 */
const useStyles = makeStyles((theme) => ({
    root: {
        '& > span': {
            margin: theme.spacing(2),
        },
    },
}));


/**
 * Traffic light app component
 */
class TrafficLight extends React.Component {

    /**
     * Specify the state object of the app
     * @param props
     */
    constructor(props) {
        super(props);
        this.state = { crossroads: [], segments: [], vehicles: {}, tab: 'simulation', alert: false, alertMessage: null, classes: useStyles, simuProgress: 0 }
    }

    /**
     * Initialize network and state app
     * @returns {Promise<void>}
     */
    async componentDidMount() {

        document.title = "Traffic Network"

        // add external link
        const link = document.createElement("link");
        link.rel = "stylesheet";
        link.href = "https://fonts.googleapis.com/icon?family=Material+Icons";
        document.head.appendChild(link);

        // initialize network
        await fetch(serveurURI + '/init', {method: 'POST'}).then(r => console.log(r.statusText))

        await this.refresh()
    }

    /**
     * Refresh the whole state context based on network system
     * @returns {Promise<void>}
     */
    async refresh(){

        // get the whole structure of traffic network
        await fetch(serveurURI + '/elements',)
            .then(res => res.json())
            .then(json => this.setState( {crossroads: json.crossroads, segments: json.segments} ))

        // get all vehicles
        await fetch(serveurURI + '/vehicles')
            .then(res => res.json())
            .then(json => this.setState({vehicles: json}))
    }

    /**
     * Update selected tab state variable and enable dynamic display
     * @param tab
     */
    async selected(tab){

        this.setState({tab: tab})

        // refresh tab element
        await this.refresh()
    }

    /**
     * Display or not modal update information
     */
    handleClose = () => this.setState({alert: false})
    handleShow = () => this.setState({alert: true})

    updateModal(data){
        if ('error' in data){
            this.setState({alertMessage: data.get('error')})
        }else{
            this.setState({alertMessage: "Element has been updated"})
        }
        this.handleShow()
    }

    /**
     *   Update crossroad (traffic light and roundabout) and segment element
     */
    async updateElement(e) {

        // avoid default click event
        e.preventDefault()

        // access to current form element
        const formNode = e.currentTarget.parentNode
        const route = formNode.dataset.kind

        // find all form fields
        const formElements = document.querySelectorAll('[form="' + formNode['name'] + '"]');

        // build input data
        let data = {}
        for (let i = 0; i < formElements.length; i++) {
            data[formElements[i]['name']] = formElements[i].value
        }

        await fetch(serveurURI + '/' + route + '/' + data['id'], {
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data),
            method: 'PUT'
        }).then(r => this.updateModal(r.json()))

        await this.refresh()
    }

    /**
     *  Add vehicle into system if form is correctly filled
     */
    async addVehicle(e){

        e.preventDefault()

        // access to current form element
        const formNode = e.currentTarget.parentNode
        const route = formNode.dataset.kind

        const formElements = document.querySelectorAll('[form="' + formNode['name'] + '"]')

        // build input data
        let data = {}
        for (let i = 0; i < formElements.length; i++) {
            data[formElements[i]['name']] = formElements[i].value
        }

        let path = await fetch(serveurURI + '/path/' + data['origin'] + '/' + data['destination']).then(res => {return res.json()})

        let inputData = {}

        inputData['time'] = data['time']
        inputData['path'] = path

        await fetch(serveurURI + '/' + route, {
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(inputData),
            method: 'POST'
        }).then(r => this.updateModal(r.json()))

        await this.refresh()
    }

    async runNetwork(e){

        e.preventDefault()

        // access to current form element
        const formNode = e.currentTarget.parentNode
        const route = formNode.dataset.kind

        const formElements = document.querySelectorAll('[form="' + formNode['name'] + '"]')

        let time = formElements[0].value
        this.state.simuProgress = 0

        // run the simulation
        for (let i = 0; i < time; i++){
            await fetch(serveurURI + '/' + route + '/' + i, {method: 'POST'})
            this.state.simuProgress = parseInt((i + 1) / time * 100)
            await this.refresh()
        }
    }

    /**
     *   Display card content specifically to Crossroad object
     */
    elemCardContent(element){
        if (element.type === 'TrafficLight') {
            return  <Form data-kind='trafficlight' name={"trafficlight-" + element.id}>
                        <Form.Control size="sm" name='id' value={element.id} type='hidden' form={"trafficlight-" + element.id}/>
                        <Form.Group>
                            <Form.Label>Orange duration</Form.Label>
                            <Form.Control size="sm" name='orangeDuration' type="number" step="0.01" defaultValue={element.orangeDuration} min='0' form={"trafficlight-" + element.id}/>
                        </Form.Group>
                        <Form.Group>
                            <Form.Label>Green duration</Form.Label>
                            <Form.Control size="sm" name='greenDuration' type="number" step="0.01" defaultValue={element.greenDuration} min='0' form={"trafficlight-" + element.id}/>
                        </Form.Group>
                        <Form.Group>
                            <Form.Label>Next passage duration</Form.Label>
                            <Form.Control size="sm" name='nextPassageDuration' type="number" step="0.01" defaultValue={element.nextPassageDuration} min='0' form={"trafficlight-" + element.id}/>
                        </Form.Group>
                        <Form.Group>
                            <Form.Label>Post red duration</Form.Label>
                            <Form.Control size="sm" name='postRedDuration' type="number" step="0.01" defaultValue={element.postRedDuration} min='0' form={"trafficlight-" + element.id}/>
                        </Form.Group>
                        <Button pill theme="secondary" type="submit" onClick={(e) => this.updateElement(e)}>
                            Update
                        </Button>
                    </Form>
        }

        if (element.type === 'Roundabout') {
            return  <Form data-kind='roundabout' name={"roundabout-" + element.id}>
                        <Form.Control size="sm" name='id' value={element.id} type='hidden' form={"roundabout-" + element.id}/>
                        <Form.Group>
                            <Form.Label>Capacity</Form.Label>
                            <Form.Control size="sm" name='capacity' type='number' defaultValue={element.capacity} form={"roundabout-" + element.id}/>
                        </Form.Group>
                        <Form.Group>
                            <Form.Label>Duration</Form.Label>
                            <Form.Control size="sm" name='duration' type='number' step="0.01" defaultValue={element.duration} form={"roundabout-" + element.id}/>
                        </Form.Group>
                        <Button pill theme="secondary" type="submit" onClick={(e) => this.updateElement(e)}>
                            Update
                        </Button>
                    </Form>
        }
    }

    elemCardHeader(element){

        if (element.type === 'TrafficLight'){
            return <div className='row'>
                        <div className='col-md-2'>
                            <div className={this.state.classes.root}>
                                <Icon style={{ fontSize: 25 }}>traffic</Icon>
                            </div>
                        </div>
                        <div className='col-md-10'>
                            <h6 style={{color:'white'}}>(#{element.id}) {element.type}</h6>
                        </div>
                    </div>
        }

        if (element.type === 'Roundabout') {
            return <div className='row'>
                <div className='col-md-2'>
                    <div className={this.state.classes.root}>
                        <Icon style={{ fontSize: 25 }}>trip_origin</Icon>
                    </div>
                </div>
                <div className='col-md-10'>
                    <h6 style={{color:'white'}}>(#{element.id}) {element.type}</h6>
                </div>
            </div>
        }
    }
    /**
     *   Display traffic light or roundabout element into configuration section
     */
    displayCardElement(element, index){
        return <Card
                    key={index}
                    bg='secondary'
                    text='white'
                    style={{ width: '18rem' }}
                >
                    <Card.Header>{this.elemCardHeader(element)}</Card.Header>
                    <Card.Body>

                    { this.elemCardContent(element) }

                    </Card.Body>
            </Card>
    }

    /**
     * Display traffic light or rounabout element during simualtion
     * @param element
     * @param index
     * @returns {*}
     */
    displayRunCardElement(element, index){
        return <Card
            key={index}
            bg='secondary'
            text='white'
            style={{ width: '24rem' }}
        >
            <Card.Header>{this.elemCardHeader(element)}</Card.Header>
            <Card.Body>

                <p style= {{color: 'white'}}><strong>Moving vehicles : </strong>{element.vehicles.map(e => e + '  ')}</p>

            </Card.Body>
        </Card>
    }

    /**
     * Display segment table line information
     * @param element
     * @param index
     * @returns {*}
     */
    displaySegmentsElement(element, index){
        return  <tr key={index}>
                    <th><Form.Control defaultValue={element.id} type='hidden' size="sm" form={'segment-' + element.id} name='id'/>{element.id}</th>
                    <th><Form.Control defaultValue={element.speed} type='number' min='1' size="sm" form={'segment-' + element.id} name='speed'/></th>
                    <th>
                        <Form data-kind='segment' name={'segment-' + element.id} id={'segment-' + element.id}>
                            <Button pill theme="secondary" size="sm" onClick={(e) => this.updateElement(e)}>Update</Button>
                        </Form>
                    </th>
                </tr>
    }

    /**
     * Display vehicle element, cannot be deleted from network
     * @param key
     * @param vehicle
     * @returns {*}
     */
    displayVehicle(key, index, vehicle){
        return  <tr key={index}>
                    <th>{vehicle.id}</th>
                    <th>{key}</th>
                    <th>{vehicle.speed}</th>
                    <th>{vehicle.position}</th>
                </tr>
    }

    /**
     * Display segment information when running information
     * @param element
     * @param index
     * @returns {*}
     */
    displayRunSegmentsElement(element, index){
        return  <tr key={index} className={element.moving_vehicles.length > 0 ? "table-secondary" : ""}>
            <th>{element.id}</th>
            <th>{element.speed}</th>
            <th>{element.moving_vehicles.map(e => e + '  ')}</th>
        </tr>
    }

    /**
     * Display vehicle element when running simulation
     * @param key
     * @param vehicle
     * @returns {*}
     */
    displayRunVehicle(key, index, vehicle){
        return  <tr key={index}>
            <th>{vehicle.id}</th>
            <th>{key}</th>
            <th>{vehicle.speed}</th>
            <th>{vehicle.position}</th>
            <th>{vehicle.step.id ? vehicle.step.id : 'No step'}</th>
            <th>
                {vehicle.step.id ?
                    <div className={this.state.classes.root}>
                        <Icon style={{ color: green[500] }}>play_circle_filled</Icon>
                    </div>
                    :
                    <div className={this.state.classes.root}>
                        <Icon style={{ color: red[500] }}>block</Icon>
                    </div>
                }
            </th>
        </tr>
    }

    /**
     * Display page content based on selected tab (configuration or simulation)
     * @returns {*}
     */
    displayContent(){

        if (this.state.tab === 'configuration') {
            return <div className="row ">

                <div className="col-md-8">
                    <div className='row'>
                        <div className='col-md-1'>
                            <div className={this.state.classes.root}>
                                <Icon style={{ fontSize: 40 }}>traffic</Icon>
                            </div>
                        </div>
                        <div className='col-md-11'>
                            <h3>Crossroads list</h3>
                        </div>
                    </div>
                    <CardDeck>
                        {this.state.crossroads.map((element, index) => (
                            this.displayCardElement(element, index)
                        ))}
                    </CardDeck>

                    <hr/>

                    <div className='row'>

                        <div className='col-md-4'>

                            <div className='row'>
                                <div className='col-md-2'>
                                    <div className={this.state.classes.root}>
                                        <Icon style={{ fontSize: 36 }}>directions_car</Icon>
                                    </div>
                                </div>
                                <div className='col-md-10'>
                                    <h3>Add vehicle</h3>
                                </div>
                            </div>

                            <Form name="addVehicle" data-kind='vehicle'>
                                <Form.Group>
                                    <Form.Label>Origin</Form.Label>
                                    <Form.Control defaultValue="5" type='number' name='origin' form='addVehicle'/>
                                </Form.Group>

                                <Form.Group>
                                    <Form.Label>Destination</Form.Label>
                                    <Form.Control defaultValue="26" type='number' name='destination' form='addVehicle'/>
                                </Form.Group>

                                <Form.Group>
                                    <Form.Label>Time</Form.Label>
                                    <Form.Control defaultValue="10" type='number' name='time' form='addVehicle'/>
                                </Form.Group>

                                <Button pill theme="secondary" onClick={(e) => this.addVehicle(e)}>Add</Button>

                            </Form>
                        </div>

                        <div className='col-md-8'>

                            <h3>Vehicles list</h3>

                            <Table striped bordered hover>
                                <thead>
                                <tr>
                                    <th>ID</th>
                                    <th>Time</th>
                                    <th>Speed</th>
                                    <th>Position</th>
                                </tr>
                                </thead>
                                <tbody>
                                {
                                    Object.keys(this.state.vehicles).length === 0 ?
                                    <tr><td>No vehicle available</td><td></td><td></td><td></td></tr> :
                                    Object.keys(this.state.vehicles).map(key => this.state.vehicles[key].map((v, i) => this.displayVehicle(key, i, v)))
                                }
                                </tbody>
                            </Table>
                        </div>
                    </div>
                </div>

                <div className="col-md-4">
                    <div className='row'>
                        <div className='col-md-1'>
                            <div className={this.state.classes.root}>
                                <Icon style={{ fontSize: 45 }}>edit_road</Icon>
                            </div>
                        </div>
                        <div className='col-md-11'>
                            <h3>Segments list</h3>
                        </div>
                    </div>

                    <Table striped bordered hover size="sm">
                        <thead>
                        <tr>
                            <th>ID</th>
                            <th>Speed</th>
                            <th>Actions</th>
                        </tr>
                        </thead>
                        <tbody>
                        {this.state.segments.map((element, index) => (
                            this.displaySegmentsElement(element, index)
                        ))}
                        </tbody>
                    </Table>
                </div>
            </div>
        }

        if (this.state.tab === 'simulation'){
            return  <div>
                        <div className='row'>

                            <div className='col-md-2'>

                                <h3>Run simulation</h3>


                                <Form.Label>Progression</Form.Label>
                                <Progress theme="secondary" value={this.state.simuProgress} />

                                <hr/>
                                <Form name="runNetwork" data-kind='run'>
                                    <Form.Group>
                                        <Form.Label>Time</Form.Label>
                                        <Form.Control defaultValue="200" type='number' name='time' form='runNetwork'/>
                                    </Form.Group>

                                    <Button pill theme="secondary" onClick={(e) => this.runNetwork(e)}>Run</Button>
                                </Form>
                            </div>
                            <div className='col-md-7'>

                                <div className='row'>
                                    <div className='col-md-1'>
                                        <div className={this.state.classes.root}>
                                            <Icon style={{ fontSize: 40 }}>traffic</Icon>
                                        </div>
                                    </div>
                                    <div className='col-md-11'>
                                        <h3>Crossroads</h3>
                                    </div>
                                </div>
                                <CardDeck>
                                    {this.state.crossroads.map((element, index) => (
                                        this.displayRunCardElement(element, index)
                                    ))}
                                </CardDeck>

                                <hr/>

                                <div className='row'>
                                    <div className='col-md-1'>
                                        <div className={this.state.classes.root}>
                                            <Icon style={{ fontSize: 36 }}>directions_car</Icon>
                                        </div>
                                    </div>
                                    <div className='col-md-11'>
                                        <h3>Vehicles</h3>
                                    </div>
                                </div>

                                <Table striped bordered hover>
                                    <thead>
                                    <tr>
                                        <th>ID</th>
                                        <th>Time</th>
                                        <th>Speed</th>
                                        <th>Position</th>
                                        <th>Step</th>
                                        <th>Moving</th>
                                    </tr>
                                    </thead>
                                    <tbody>
                                    {
                                        Object.keys(this.state.vehicles).length === 0 ?
                                        <tr><td>No vehicle available</td><td></td><td></td><td></td><td></td><td></td></tr> :
                                        Object.keys(this.state.vehicles).map(key => this.state.vehicles[key].map((v, i) => this.displayRunVehicle(key, i, v)))
                                    }
                                    </tbody>
                                </Table>
                            </div>
                            <div className='col-md-3'>

                                <h3>Segments</h3>

                                <Table striped bordered hover size="sm">
                                    <thead>
                                    <tr>
                                        <th>ID</th>
                                        <th>Speed</th>
                                        <th>Moving vehicles</th>
                                    </tr>
                                    </thead>
                                    <tbody>
                                    {this.state.segments.map((element, index) => (
                                        this.displayRunSegmentsElement(element, index)
                                    ))}
                                    </tbody>
                                </Table>
                            </div>
                        </div>
                    </div>
        }

        if (this.state.tab === 'overview'){

            return  <div>
                        <h3>Network overview</h3>

                        <center >
                            <img alt="" src={"traffic_1.svg"} width="60%" />
                        </center>
                    </div>
        }
    }

    /**
     * Render the whole app page
     * @returns {*}
     */
    render() {
        return (

            <div>

                <Navbar type="dark" theme="secondary" expand="md" style={{marginBottom: 5}}>
                    <NavbarBrand href="#">Traffic light</NavbarBrand>

                    <Collapse  navbar>
                        <Nav navbar>
                            <NavItem>
                                <NavLink href="#simulation" className={(this.state.tab === 'simulation' ? 'active' : '')}
                                          onClick={() => this.selected('simulation')}>Simulation</NavLink>
                            </NavItem>
                            <NavItem>
                                <NavLink href="#configuration" className={(this.state.tab === 'configuration' ? 'active' : '')}
                                          onClick={() => this.selected('configuration')}>Configuration</NavLink>
                            </NavItem>

                            <NavItem>
                                <NavLink href="#overview" className={(this.state.tab === 'overview' ? 'active' : '')}
                                         onClick={() => this.selected('overview')}>Overview</NavLink>
                            </NavItem>
                        </Nav>
                    </Collapse>
                </Navbar>

                <Modal show={this.state.alert} onHide={this.handleClose}>
                    <Modal.Header closeButton>
                        <Modal.Title>Update information</Modal.Title>
                    </Modal.Header>
                    <Modal.Body>{this.state.alertMessage}</Modal.Body>
                    <Modal.Footer>
                        <Button pill theme="secondary" onClick={this.handleClose}>
                            Close
                        </Button>
                    </Modal.Footer>
                </Modal>

                <div className="container-fluid">
                    {this.displayContent()}
                </div>
            </div>
        );
    }
}


/**
 * Use the traffic light component into DOM
 */
ReactDOM.render(
    <TrafficLight />,
    document.getElementById('root')
);