package eu.easyminer.discretization;

/**
 * Created by propan on 12. 3. 2017.
 */
public interface Interval {

    Double getLeftBoundValue();

    Double getRightBoundValue();

    Boolean isLeftBoundOpened();

    Boolean isRightBoundOpened();

    Boolean isLeftBoundClosed();

    Boolean isRightBoundClosed();

}
