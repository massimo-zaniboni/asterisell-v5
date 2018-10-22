# Communication channel

The Communication channel identifies the exact method used for routing a VoIP call to the external telephone number.

It can be used for:

  - associating to a [cdr] its [communication-channel-type]
  - associating to a [cdr] the [vendor] responsible of the routing
  - as condition to use during rating of a [cdr]

Usually there can be many communication channels, but few [vendor] and [communication-channel-type].

The usual Asterisell rating workflow is:

  - import [cdr] from a [cdrs-provider]
  - associate to the [cdr] the [communication-channel]
  - use it for deriving the corresponding [communication-channel-type] and [vendor]
  - use this information for rating the [cdr]
