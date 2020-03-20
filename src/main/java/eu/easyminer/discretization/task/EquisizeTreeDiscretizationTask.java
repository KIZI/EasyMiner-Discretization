package eu.easyminer.discretization.task;

import eu.easyminer.discretization.DiscretizationTask;
import eu.easyminer.discretization.Support;

/**
 * Created by propan on 31. 3. 2017.
 */
public interface EquisizeTreeDiscretizationTask extends DiscretizationTask {

    Support getMinSupport();

    int getArity();

    boolean inParallel();

}