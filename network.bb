;  / _| ___(_) __| | ___ _ __
; | |_ / _ \ |/ _` |/ _ \ '__|
; |  _|  __/ | (_| |  __/ |
; |_|  \___|_|\__,_|\___|_|
;
; visit https://feider.space
; and https://github.com/feider/
;
; Written as an absolutely useful project 
; during the corona crisis in April 2020
; I started real programming in this language 
; around 2002, when I was 12 years old.
; BB is really not perfect for stuff like this,
; But it was fun coming back :)
;
; You can use Blitz3D-ng to run it
; https://github.com/blitz3d-ng/blitz3d-ng
;
; Also a shoutout and thanks to the blitzforum.de community!
; There I was able to make my first steps as a programmer,
; chasing my dreams of creating the games I always wanted to play :)
; 
;
; This piece of software creates a feedforward neural network
; The network has several layers and several nodes
; For the value of every node is the weighted sum 
; of the outputs of all nodes of the previous layer, 
; put through an activation function.

; Stuff you can play aroud with easily is marked with CHANGEME


Graphics 600, 800, 0, 2 ; AAH I REMEMBER TYPING THIS 


; #########
; General defines for the network, pls do not mess with
Const ACT_NONE = 0 ; No activation function, just outputs the input
Const ACT_RELU = 1 ; Rectified Linear Unit, outputs 0 for intputs <= 0, outputs the input else
                    ; Or does it output 0 for inputs < 0 and the input else? Who knows!
; #########


; ################################
; Define the network architecture
; first define the number of layers
; You can mess with this
; CHANGEME
Const n_layers = 3
Const input_dimension = 1
Const output_dimension = 1
; I just remembered that Dims are always global in BB lol
Dim layers(n_layers-1)      ; Number of nodes per layer
Dim node_offset(n_layers-1) ; Node offset per ayer
Dim activations(n_layers-1) ; Activation functions of the layers
; now define the number of neurons per layer
; and the activation function
layers(0) = input_dimension
layers(1) = 500
layers(2) = output_dimension
activations(0) = ACT_NONE ; Input
activations(1) = ACT_RELU ; Hidden Layer
activations(2) = ACT_NONE ; Output
; ################################


; ################################
; The network is defined, now let's create it
; now reserve space for the node values
Global n_nodes = 0
; count the layers
For l = 0 To n_layers-1
    node_offset(l) = n_nodes
    current_nodes = layers(l)
    n_nodes = n_nodes + current_nodes
Next
Print "Allocating space for "+n_nodes+" nodes"
Dim nodes#(n_nodes) ; node values for input, hidden layers and output
Dim layer_of_node(n_nodes) ; the layer of a node
; Count the weights
Global n_weights = 0
For l = 0 to n_layers-1 
    For n = 0 To layers(l)-1 ; for every node in the layer
        layer_of_node(n+node_offset(l)) = l
        If l > 0 ; else there is no need to count weights
            n_weights = n_weights + layers(l-1) + 1 ; Add one extra weight per node
        End If
    Next
Next
Print "Allocating space for "+n_weights+" weights"
Dim weights#(n_weights)
Dim node_of_weight(n_weights)
; Calculate and store the weight offsets
Dim weight_offset(n_nodes) ; weight offset per node"
current_weight_offset = 0
weight_offset(0) = 0
current_weights = 0
For n = 0 To n_nodes-1:
    If n > 0
        ; current_weights have been set in the last interation
        weight_offset(n) = weight_offset(n-1)+current_weights
    End If
    l = layer_of_node(n)
    If l > 0
        current_weights = layers(l-1)+1
    End If
Next
; Every weight is initialized with a random value
For w = 0 To n_weights-1
    weights(w) = Rnd(-0.1, 0.1)
Next
; To train the network we will have to calculate the gradient for every weight
; So we also reserve this space
; All this could be solved more elegantly
; By storing only the currently needed gradient
; But this will be left as an exercise for the reader :^)
Print "Allocating space for "+n_weights+" gradients"
Dim gradients#(n_weights)
For g_id = 0 To n_weights-1
    gradients(g_id) = 0
Next
Global output_offset = n_nodes-output_dimension
; Now our network is created and stored in memory
; #################################



; #################################
; calculate the output node
; Test untrained network
Print "Press key for a test run of the untrained network!"
WaitKey()
For i = -10 To 10
    x# = i
    y# = to_approximate(x)
    nodes(0) = x ; set network input.
    calculate_network_output()
    res# = nodes(output_offset) ; get network result
    Print "f("+i+") = "+res+ ", correct: "+y+" loss: "+loss(y, res)
Next
; You will notice that the output does not make sense.
; That is why we want to train it and make its error smaller
; The error is represented by the loss function
; #################################


; #################################
; Pretty bad, so let us train the network
Print "Press key to train the network!"
WaitKey()
; you can play around with these values
; CHANGEME
lr# = 0.00001 ; the learning rate
epochs = 10 
steps_in_epoch = 1000 
momentum# = 0.9 
For epoch = 0 To epochs
    mean_loss# = 0
    For s = 0 To steps_in_epoch-1
        x# = Rnd#(-12, 12) ; Train with random values
        y# = to_approximate(x)
        nodes(0) = x ; set network input
        calculate_network_output() ; without using the network, we can't calculate the loss
        res# = nodes(output_offset) ; get result
        mean_loss = mean_loss+loss(y, res) ; calculate mean loss for displaying it
        calculate_network_gradients(loss_gradient(y, res), momentum) ; calculate the gradients from the loss
        gradient_descent(lr) ; change the weights according to the gradients
    Next
    mean_loss = mean_loss/steps_in_epoch
    print "epoch "+epoch + " mean loss: "+mean_loss
Next
; now our loss function should be much smaller
; If it isn't try playing around with the parameters
; #################################


; #################################
; The network is trained
Print "Press key for a test run of the trained network!"
WaitKey()
For i = -10 To 10
    x# = i
    y# = to_approximate(x)
    nodes(0) = x ; set network input
    calculate_network_output()
    res# = nodes(output_offset) ; get network results
    Print "f("+i+") = "+res+ ", correct: "+y+" loss: "+loss(y, res)
Next
Print "Press key to end the program!"
WaitKey()
; See? much better now!
; #################################


End
; bye!


; #################################
; FUNCTIONS

; calculate_network_output
; INPUT:
; none
; RETURNS:
; none
; 
; Calculates the network output by traversing the layers
; And nodes in the layers. Calculates the nodes values 
; by using their weights and the nodes of the previous layer
Function calculate_network_output()
weights_offset = 0
For l = 1 To n_layers-1 ; Skip the first layer, we already have inputs
    For n = 0 To layers(l)-1
        n_id = n+node_offset(l) ; this is the global node id
        ;now, for every weight in the layer, multiply the weight with the output of the previous node
        res# = 0
        For w = 0 To layers(l-1)-1 ; we have as many regular weights as nodes in the previous layer
            w_val# = weights(w+weight_offset(n_id))
            n_val# = nodes(w+node_offset(l-1))
            res# = res# + (w_val*n_val)
        Next
        ;now for the +1
        w = layers(l-1)
        w_val# = weights(w+weight_offset(n_id))
        n_val# = 1.0
        res# = res# + (w_val*n_val)
        ; now the activation function
        If activations(l) = ACT_RELU
            res# = ReLU(res)
        End If
        nodes(n_id) = res
    Next
Next 
End Function

; calculate_network_gradients
; INPUT:
; loss_grad# - the gradient of the loss function
; momentum#  - the momentum
; RETURNS:
; none
;
; Calculates a gradient value for every weight in the network
; Using momentum means, that gradients are gradually changed
; So the gradient from the last computation "bleeds" into this one
; Set the momentum=0 for simple gradient descent
Function calculate_network_gradients(loss_grad#, momentum#)
    For l = n_layers-1 To 1 Step -1 ; The first layer has no incoming weights
        For n = 0 To layers(l)-1 ; All nodes in the current layer
            n_id = n+node_offset(l)
            grad_out# = 0
            ; first the special case, we're in the last layer
            If l = n_layers-1
                ; gradient coming from the loss function
                grad_out# = loss_grad
            Else
                ; Calculate the gradient coming from the loss function direction
                ; For every node in the next layer (ng) take the n-th gradient
                For ng = 0 To layers(l+1)-1
                    ; calculate the node
                    ng_id = ng+node_offset(l+1)
                    ; weight offsets = gradient offsets
                    ; calculate the weight offset of the node
                    g_id = weight_offset(ng_id)
                    g_id = g_id + n
                    grad_out# = grad_out# + gradients(g_id)
                Next
            End If
            ; now we know the gradient we have to multiply our results with
            ; for every weight of this node
            For w = 0 To layers(l-1)-1
                w_id = weight_offset(n_id)+w
                ; compute gradient
                ; nodes(n) is already zero if we have a ReLU activation
                ; with negative node value
                grad# = nodes(n_id)*weights(w_id) 
                grad# = grad*grad_out
                gradients(w_id) = (momentum*gradients(w_id)) + ((1.0-momentum)*grad#)
            Next
            ; now we also need the gradient for the +1
            w = layers(l-1)
            w_id = weight_offset(n_id)+w
            grad# = 1.0*weights(w_id)
            grad# = grad*grad_out
            gradients(w_id) = (momentum*gradients(w_id)) + ((1.0-momentum)*grad#)
        Next
    Next
End Function

; gradient_descent
; INPUT:
; lr# - learning rate
; RETURNS
; none
;
; Changes the network weights according to the computed gradient
; and according to the learning rate
; also places a hard cap on weight size. We don't use any
; nice weight regularization here, so that's a replacement for that
Function gradient_descent(lr#)
    mean_weights# = 0
    For wg = 0 To n_weights-1
        weights(wg) = weights(wg) - (lr*gradients(wg))
        weights(wg) = max(-1, weights(wg))
        weights(wg) = min(1, weights(wg))
        mean_weights = mean_weights + weights(wg)
        
    Next
    mean_weights = mean_weights / Float(n_weights)
    ;print "mean weights: "+mean_weights
End Function

; ReLU
; INPUT:
; in# - node output
; RETURNS:
; ReLU value
;
; Rectified Linear Unit is a common neural network activation function
; if the input is smaller than 0, return 0
; else return the input
Function ReLU#(in#)
    Return max(in, 0)
End Function

; loss
; INPUT:
; y_true# - correct value
; y_pred# - predicted value
; RETURNS:
; mean squared error
;
; Simple Mean Squared Error Loss
; Is always positive, is more extreme for bigger errors
Function loss#(y_true#, y_pred#)
    diff# = y_true-y_pred
    Return diff*diff
End Function

; loss_gradient
; INPUT:
; y_true# - correct value
; y_pred# - predicted value
; RETURNS:
; gradient of our loss function
Function loss_gradient#(y_true#, y_pred#)
    gradient# = -2*(y_true-y_pred)
    Return gradient
End Function

; simple min function
Function min#(a#, b#)
    If a < b
        Return a
    Else
        Return b
    EndIf
End Function

; simple max function
Function max#(a#, b#)
    If a > b
        Return a
    Else
        Return b
    EndIf
End Function

; to_approximate
; INPUT:
; x# - the function input value
; RETURNS:
; the value that the function computes
;
; This is the function we want our network to approximate
; I used a simple x^2, you can try different ones
; CHANGEME
Function to_approximate#(x#)
    Return x*x
End Function
; #################################  <- I had to insert this or I would have gone mad
