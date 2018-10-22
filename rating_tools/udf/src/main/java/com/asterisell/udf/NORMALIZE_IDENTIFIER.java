// SPDX-License-Identifier: GPL-3.0-or-later

package com.asterisell.udf;

import org.apache.pig.EvalFunc;
import org.apache.pig.PigException;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.data.Tuple;

import java.io.IOException;

/**
 * Transform a name into an identifier.
 */
public class NORMALIZE_IDENTIFIER extends EvalFunc<String> {

    public String exec(Tuple input) throws IOException {
        if (input.size() != 1) {
            int errCode = 2107;
            String msg = "NORMALIZE_IDENTIFIER expected one input but received " + input.size() + " inputs.";
            throw new ExecException(msg, errCode, PigException.BUG);
        }

        try {
            String s = (String) input.get(0);
            return s.replace(' ', '-');
        } catch (Exception e) {
            throw new IOException("Caught exception processing input row ", e);
        }
    }
}
