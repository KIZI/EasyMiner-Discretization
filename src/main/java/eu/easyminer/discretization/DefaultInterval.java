package eu.easyminer.discretization;

/**
 * Created by propan on 12. 3. 2017.
 */
public interface DefaultInterval {

    Double getLeftBoundValue();

    Double getRightBoundValue();

    Boolean isLeftBoundOpened();

    Boolean isRightBoundOpened();

    Boolean isLeftBoundClosed();

    Boolean isRightBoundClosed();

}
