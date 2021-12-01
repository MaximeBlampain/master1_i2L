// Test d'exemple par défaut :
describe('The Traffic web site home page', () => {
    it('successfully loads', () => {
        cy.visit('/')
    })

})

describe('Scenario 1', () => {
    it('load configuration page', () => {
        cy.visit('/')
        cy.get('a[href ="#configuration"]').click()
        cy.url().should('include', '/#configuration')
    })
})

describe('Scenario 2', () => {
    it('count 28 segments', () => {
        cy.get('.col-md-4>table>tbody>tr')
            .should('have.length', 28)
    })
})

describe('Scenario 3', () => {
    it('count 0 vehicle', () => {
        cy.get('.col-md-8 table>tbody>tr>td')
            .eq(0)
            .should('contain', 'No vehicle available')
    })
})

describe('Scenario 4', () => {
    it('change segment 5 speed', () => {
        cy.get('.col-md-4 table>tbody>tr')
            .eq(4)
            .find('th')
            .eq(1)
            .find('input')
            .clear()
            .type('30')
            .should('have.value', '30')
    })

    it('see modal appear', () => {
        cy.get('.col-md-4 table>tbody>tr')
            .eq(4)
            .find('th')
            .eq(2)
            .click()
        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })
})

describe('Scenario 5', () => {
    it('change capacity from 3 to 4', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(2)
            .find('.card-body>form>.form-group')
            .eq(0)
            .find('input[name="capacity"]')
            .clear()
            .type('4')
            .should('have.value', '4')
    })

    it('change duration from 10 to 15', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(2)
            .find('.card-body>form>.form-group')
            .eq(1)
            .find('input[name="duration"]')
            .clear()
            .type('15')
            .should('have.value', '15')
    })

    it('update values',() => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(2)
            .find('button[type="submit"]')
            .click()
        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })
})

describe('Scenario 6',() => {
    it('select trafficLight n°29', () => {
        cy.get('.col-md-8>.card-deck>div')
            .contains('(#29)')
    })

    it('change value of orange duration', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(0)
            .find('input[name="orangeDuration"]')
            .clear()
            .type('4')
            .should('have.value', '4')
    })

    it('change value of green duration', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(0)
            .find('input[name="greenDuration"]')
            .clear()
            .type('40')
            .should('have.value', '40')
    })

    it('change value of next passage duration', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(0)
            .find('input[name="nextPassageDuration"]')
            .clear()
            .type('8')
            .should('have.value', '8')
    })

    it('save changes', () => {
        cy.get('.col-md-8>.card-deck>div')
            .eq(0)
            .find('button[type="submit"]')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })
})

describe('Scenario 7', () => {
    it('add first vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('5')
            .should('have.value', '5')
    })

    it('add first vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('26')
            .should('have.value', '26')
    })

    it('add first vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('50')
            .should('have.value', '50')
    })

    it('add first vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add second vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('19')
            .should('have.value', '19')
    })

    it('add second vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('8')
            .should('have.value', '8')
    })

    it('add second vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('200')
            .should('have.value', '200')
    })

    it('add second vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add third vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('27')
            .should('have.value', '27')
    })

    it('add third vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('2')
            .should('have.value', '2')
    })

    it('add third vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('150')
            .should('have.value', '150')
    })

    it('add third vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('should have 3 vehicle on the simulation', () => {
        cy.get('.col-md-8 table>tbody>tr')
            .should('have.length', 3)
    })
})

describe('Scenario 8', () => {
    it('vehicles are stopped',() => {
        cy.get('a[href="#simulation"]').click()

        cy.get('.col-md-7 > table > tbody').as("vehicleTable")
        cy.get("@vehicleTable").find('tr').eq(0).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(1).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(2).find('th').eq(5).find('div').find('span').should('contain', "block")
    })

    it('change simulation time then run',() => {
        cy.get('.col-md-2>form[name="runNetwork"]>.form-group>input[name="time"]')
            .clear()
            .type('120')
            .should('have.value', '120')

        // Launch simulation
        cy.get('.col-md-2>form[name="runNetwork"]>button')
            .contains("Run")
            .click()

        // Wait the end of the simulation
        cy.wait(20*1000)
    })

    it('check progressbar is finished', () => {
        cy.get('.col-md-2>.progress>.progress-bar')
            .should('have.attr', 'aria-valuenow', 100)
    })

    it('look if only one car is moving', () => {
        cy.get('.col-md-7 > table > tbody').as("vehicleTable")
        cy.get("@vehicleTable").find('tr').eq(0).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(1).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(2).find('th').eq(5).find('div').find('span').should('contain', "play_circle_filled")
    })
})

describe('Scenario 9', () => {
    it('reload page & check no vehicles',() => {
        cy.reload()
        cy.get('.col-md-7>table>tbody>tr>td')
            .eq(0)
            .should('contain', 'No vehicle available')
    })


    it('add first vehicle origin', () => {
        cy.get('a[href ="#configuration"]').click()
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('5')
            .should('have.value', '5')
    })

    it('add first vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('26')
            .should('have.value', '26')
    })

    it('add first vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('50')
            .should('have.value', '50')
    })

    it('add first vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add second vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('19')
            .should('have.value', '19')
    })

    it('add second vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('8')
            .should('have.value', '8')
    })

    it('add second vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('200')
            .should('have.value', '200')
    })

    it('add second vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add third vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('27')
            .should('have.value', '27')
    })

    it('add third vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('2')
            .should('have.value', '2')
    })

    it('add third vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('150')
            .should('have.value', '150')
    })

    it('add third vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('should have 3 vehicle on the simulation', () => {
        cy.get('.col-md-8 table>tbody>tr')
            .should('have.length', 3)
    })

    it('change simulation time then run',() => {
        cy.get('a[href="#simulation"]').click()
        cy.get('.col-md-2>form[name="runNetwork"]>.form-group>input[name="time"]')
            .clear()
            .type('500')
            .should('have.value', '500')

        // Launch simulation
        cy.get('.col-md-2>form[name="runNetwork"]>button')
            .contains("Run")
            .click()

        // Wait the end of the simulation
        cy.wait(60*1000)
    })

    it('vehicles must be stopped',() => {
        cy.get('.col-md-7 > table > tbody').as("vehicleTable")
        cy.get("@vehicleTable").find('tr').eq(0).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(1).find('th').eq(5).find('div').find('span').should('contain', "block")
        cy.get("@vehicleTable").find('tr').eq(2).find('th').eq(5).find('div').find('span').should('contain', "block")

    })
})

describe('Scenario 10', () => {

    it('reload page & add first vehicle origin', () => {
        cy.reload()
        cy.get('a[href ="#configuration"]').click()
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('5')
            .should('have.value', '5')
    })

    it('add first vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('26')
            .should('have.value', '26')
    })

    it('add first vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('50')
            .should('have.value', '50')
    })

    it('add first vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('inform user that changes are registered', () => {
        cy.get('.modal-dialog>.modal-content>.modal-body')
            .contains('Element has been updated')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add second vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('5')
            .should('have.value', '5')
    })

    it('add second vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('26')
            .should('have.value', '26')
    })

    it('add second vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('80')
            .should('have.value', '80')
    })

    it('add second vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('add third vehicle origin', () => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="origin"]')
            .clear()
            .type('5')
            .should('have.value', '5')
    })

    it('add third vehicle destination',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="destination"]')
            .clear()
            .type('26')
            .should('have.value', '26')
    })

    it('add third vehicle time',() => {
        cy.get('form[name="addVehicle"]')
            .find('input[name="time"]')
            .clear()
            .type('80')
            .should('have.value', '80')
    })

    it('add third vehicle',() => {
        cy.get('form[name="addVehicle"]')
            .find("button")
            .contains('Add')
            .click()

        // See if modal appear
        cy.get('.modal-dialog')
            .should('be.visible')
    })

    it('close modal', () => {
        cy.get('.modal-footer>button')
            .click()
        cy.contains('Element has been updated')
            .should('be.not.visible')
    })

    it('should have 3 vehicle on the simulation', () => {
        cy.get('.col-md-8 table>tbody>tr')
            .should('have.length', 3)
    })

    it('change simulation time then run',() => {
        cy.get('a[href="#simulation"]').click()
        cy.get('.col-md-2>form[name="runNetwork"]>.form-group>input[name="time"]')
            .clear()
            .type('200')
            .should('have.value', '200')

        // Launch simulation
        cy.get('.col-md-2>form[name="runNetwork"]>button')
            .contains("Run")
            .click()

        // Wait the end of the simulation
        cy.wait(25*1000)
    })

    it('see vehicle 1 & 2 on S29 & vehicle 3 on S17', () => {
        cy.get('.col-md-7 > table > tbody').as("vehicleTable")
        cy.get("@vehicleTable").find('tr').eq(0).find('th').eq(4).should('contain', "29")
        cy.get("@vehicleTable").find('tr').eq(1).find('th').eq(4).should('contain', "29")
        cy.get("@vehicleTable").find('tr').eq(2).find('th').eq(4).should('contain', "17")

    })
})