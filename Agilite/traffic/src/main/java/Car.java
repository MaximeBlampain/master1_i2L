public class Car {
    // Variables
    private float _speed;
    private float _traveledTime = 0;
    private float _stopTime;
    private Segment _segment;


    // Constructor
    public Car(float speed){
        this.setSpeed(speed);
    }

    // Getters & Setters
    public float getSpeed(){
        return this._speed;
    }
    public void setSpeed(float speed) {
        this._speed = speed;
    }

    public Segment getSegment() {
        return this._segment;
    }
    public void setSegment(Segment segment) {
        this._segment = segment;
    }

    public float getTraveledTime() {
        return _traveledTime;
    }

    public void setStopTime(float _stopTime) {
        this._stopTime = _stopTime;
    }

    public float getStopTime() {
        return _stopTime;
    }

    // Methods
    public void drive(){
        this._traveledTime += this._speed;
    }
    public void waitStop(){
        this._stopTime --;
    }
    public void switchSegment(Segment newSegment){
        this._segment.setCar(null);

        this._segment = newSegment;
        this._speed = newSegment.getConstantSpeed();
        this._segment.setCar(this);
        this._traveledTime = 0;
    }

}
