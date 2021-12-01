import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SegmentTest {

    @Test
    public void checkSegmentSpeed() {
        Segment mySegment = new Segment(10);

        assertEquals(mySegment.getConstantSpeed(), 10, 0.001);
    }

    @Test
    public void checkSegmentTime() {
        Segment mySegment = new Segment(10,180);

        float time = 180/10;

        assertEquals(time,mySegment.getTime(),0.001);
    }

    @Test
    public void checkSegmentHaveAStop() {
        Segment mySegment = new Segment(10,1000, null, true);

        assertEquals(true, mySegment.getHaveAStop());
    }

    @Test
    public void checkStopDuration() {
        Segment mySegment = new Segment(10,1000, null, true);
        mySegment.setStopDuration(3);

        assertEquals(3,mySegment.getStopDuration(), 0.001);
    }

}
