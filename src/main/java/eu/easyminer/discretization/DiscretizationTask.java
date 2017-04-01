package eu.easyminer.discretization;

/**
 * Created by propan on 12. 3. 2017.
 */
public interface DiscretizationTask {

    /**
     * This property determines how much memory discretization algorithms can consume.
     *
     * @return buffer size in bytes
     */
    int getBufferSize();

}
