import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class GameTest {
    @Test
    public void checkGameExist() {
        Game myGame = new Game();
        assertEquals(myGame.getNbCoup(), 2);
    }
}
