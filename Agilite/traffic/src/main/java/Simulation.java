public class Simulation {
    // Variables
    private int _actualTime = 0;
    private int _maxTime = 0;
    private Car _car;
    private Segment _segment;

    // Constructors
    public Simulation(int maxTime, Car oneCar) {
        this._maxTime = maxTime;
        this._car = oneCar;
        this._segment = oneCar.getSegment();
    }
    // Getters/Setters

    // Methods
    public void runSim() {
        for (int time = this._actualTime ; time < this._maxTime ; time++){
            this.checkIfNeedToEject(this._segment);

            if(_car.getTraveledTime() < _segment.getTime()) {
                _car.drive();
            } else {
                if(_segment.getHaveAStop() && _car.getStopTime() > 0){
                    _car.waitStop();
                } else {
                    if(_segment.getNextSegment()!= null && _segment.getNextSegment().getCar()==null)
                        _car.switchSegment(_segment.getNextSegment());
                }

            }
        }
    }
    private void checkIfNeedToEject(Segment segment){

        if(segment.getCar().getTraveledTime() >= segment.getTime()){
            if(segment.getHaveAStop()){
                if(segment.getCar().getStopTime() == 0 && segment.getNextSegment() == null){
                    segment.setCar(null);
                    this._car.setSegment(null);
                    this._segment = segment;
                    this._maxTime = this._actualTime;
                }
            }else{
                segment.setCar(null);
                this._car.setSegment(null);
                this._segment = segment;
                this._maxTime = this._actualTime;
            }
        }
    }
}
