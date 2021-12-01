import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CarTest {
    @Test
    public void checkCarSpeedOnSegment() {
        Car myCar = new Car(10);
        Segment mySegment = new Segment(10);

        mySegment.setCar(myCar);

        assertEquals(10, myCar.getSpeed(), 0.001);
    }

    @Test
    public void checkSwitchSegment() {
        Segment secondSegment = new Segment(20,2000);
        Segment mySegment = new Segment(10,1800, secondSegment);
        Car myCar = new Car(mySegment.getConstantSpeed());

        mySegment.setCar(myCar);
        myCar.setSegment(mySegment);

        Simulation mySimulation = new Simulation(22,myCar);
        mySimulation.runSim();

        //check if myCar is on the 2nd segment
        assertEquals(secondSegment, myCar.getSegment());

    }
}
