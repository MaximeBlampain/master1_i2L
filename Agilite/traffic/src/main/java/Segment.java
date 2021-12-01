public class Segment {
    // Variables
    private float _constantSpeed;
    private float _distance;
    private float _time;
    private Car _car = null;
    private Segment _nextSegment = null;
    private boolean _haveAStop;
    private int _stopDuration;

    // Constructors
    public Segment(float speed){
        this.setConstantSpeed(speed);
    }
    public Segment(float speed, float distance){
        this.setConstantSpeed(speed);
        this.setTime(speed, distance);
    }
    public Segment(float speed, float distance, Segment nextSegment){
        this.setConstantSpeed(speed);
        this.setDistance(distance);
        this.setTime(speed, distance);
        this.setNextSegment(nextSegment);
    }

    public Segment(float speed, float distance, Segment nextSegment, boolean haveAStop){
        this.setConstantSpeed(speed);
        this.setDistance(distance);
        this.setTime(speed, distance);
        this.setNextSegment(nextSegment);
        this.setHaveAStop(haveAStop);
    }

    // Getters / Setters
    public float getConstantSpeed(){
        return this._constantSpeed;
    }
    public void setConstantSpeed(float constantSpeed){
        this._constantSpeed = constantSpeed;
    }

    public void setCar(Car car) {
        this._car = car;
    }
    public Car getCar() {
        return _car;
    }

    public void setDistance(float distance) {
        this._distance = distance;
    }

    public float getTime() {
        return this._time;
    }
    public void setTime(float speed, float distance) {
        this._time = distance / speed;
    }

    public Segment getNextSegment() {
        return this._nextSegment;
    }
    public void setNextSegment(Segment nextSegment) {
        this._nextSegment = nextSegment;
    }

    public boolean getHaveAStop() {
        return _haveAStop;
    }
    public void setHaveAStop(boolean haveAStop){
        this._haveAStop = haveAStop;
    }

    public int getStopDuration() { return _stopDuration; }
    public void setStopDuration(int stopDuration) { this._stopDuration = stopDuration; }
}
