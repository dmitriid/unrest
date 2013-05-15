function PandocController($scope, $http, $timeout) {
    var is_change_requested = false;

    $scope.wrkg = " ";
    $scope.is_dirty = false;

    $scope.dirty = function(){
        $scope.is_dirty = true;
    };

    $scope.change = function(){
        change();
    }

    var requestChange = function(){
        is_chage_requested = true;
        $timeout(requestChange, 5);
        change();
    };

    var change = function(){
        if(!$scope.is_dirty){
  //          return;
        }
        $scope.wrkg = "Wrkg..."
        is_change_requested = false;
        $.post( "/"
              , {rawText: $scope.rawTextModel}
              ).done(parse_incoming);
    };

    var parse_incoming = function(data, status, jqXHR) {
        $scope.rendered = data;
        $scope.wrkg = " ";
        $scope.rendered = data;
        if(is_change_requested){
            $scope.is_dirty = true;
            change();
        } else {
            $scope.is_dirty = false;
        }
    };

//    $timeout(requestChange, 1);
}
