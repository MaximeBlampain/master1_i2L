import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SimulationTest {

    @Test
    public void checkCarIsStopped() {
        Segment secondSegment = new Segment(20,2000, null, false);
        Segment mySegment = new Segment(10,1800, secondSegment, true);
        Car myCar = new Car(mySegment.getConstantSpeed());

        mySegment.setCar(myCar);
        myCar.setSegment(mySegment);

        mySegment.setStopDuration(3);
        myCar.setStopTime(3);

        Simulation mySim = new Simulation(20, myCar);
        mySim.runSim();

        assertEquals(mySegment, myCar.getSegment());
        assertEquals(1, myCar.getStopTime());
    }

    @Test
    public void checkCarNeedToWaitMore() {
        Segment secondSegment = new Segment(20,5000);
        Segment mySegment = new Segment(10,1800, secondSegment);

        Car myCar = new Car(mySegment.getConstantSpeed());
        Car secondCar = new Car(secondSegment.getConstantSpeed());

        mySegment.setCar(myCar);
        myCar.setSegment(mySegment);
        mySegment.setStopDuration(3);

        secondCar.setSegment(secondSegment);
        secondSegment.setCar(secondCar);

        Simulation mySim = new Simulation(20, myCar);
        Simulation secondSim = new Simulation(20, secondCar);

        mySim.runSim();
        secondSim.runSim();

        assertEquals(mySegment, myCar.getSegment());
        assertEquals(secondSegment, secondCar.getSegment());
    }

    @Test
    public void checkEjectTimeoutSegment() {
        Segment secondSegment = new Segment(10,2100);
        Segment mySegment = new Segment(10,1800);

        Car myCar = new Car(mySegment.getConstantSpeed());
        Car secondCar = new Car(secondSegment.getConstantSpeed());

        mySegment.setCar(myCar);
        myCar.setSegment(mySegment);

        secondCar.setSegment(secondSegment);
        secondSegment.setCar(secondCar);

        Simulation mySim = new Simulation(20,myCar);
        Simulation secondSim = new Simulation(20, secondCar);

        mySim.runSim();
        secondSim.runSim();

        assertEquals(null, myCar.getSegment());
        assertEquals(secondSegment, secondCar.getSegment());

    }

}
