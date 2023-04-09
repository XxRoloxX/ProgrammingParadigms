import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Main {


    public static void christmasCard(){
        Scene scene = new Scene(400,300);
        ItemInterface segment = new Segment(new Point(50,50),new Point(100,100));
        ///segment.setPrecision(2);
        ItemInterface circle = new Circle(false,new Point(200,30),30);

        ItemInterface snowmanHead = new Circle(false, new Point(300,70),20);
        ItemInterface snowmanBody = new BoundingBoxDecorator(new Circle(false, new Point(290,110),30));
        ItemInterface snowmanLegs = new Circle(false, new Point(280,170),40);
        ItemInterface snowmanFace = new ComplexItem();
        snowmanFace.setDeph(1000);

        Point[] mouthPoints = new Point[4];
        mouthPoints[0] = new Point(305,90);
        mouthPoints[1] = new Point(315,100);
        mouthPoints[2] = new Point(325,100);
        mouthPoints[3] = new Point(335,90);
        ItemInterface snowmanMouthBezier = new Bezier(mouthPoints);
        snowmanMouthBezier.setPrecision(0.01);
        ItemInterface snowmanLeftEye = new Circle(false,new Point(305,75),5);
        ItemInterface snowmanRightEye = new Circle(false, new Point(315,75),5);
        ItemInterface snowmanNose = new Triangle(true, new Point(292,69), new Point(313,84), new Point(317,93));
        snowmanNose.setCharToDraw('?');
        snowmanNose.setDeph(1000);


        ((ComplexItem)snowmanFace).addItem(snowmanMouthBezier);
        ((ComplexItem)snowmanFace).addItem(snowmanLeftEye);
        ((ComplexItem)snowmanFace).addItem(snowmanRightEye);
        ((ComplexItem)snowmanFace).addItem(snowmanNose);

        Item hatBase = new Rect(new Point(290,65),true,60,10);
        //Item hatBase = new Rect(snowmanHead.getPosition().getX())
        Item hatTop = new Rect(new Point(305,35),true,30,30);
        hatBase.setCharToDraw('=');
        hatTop.setCharToDraw('=');




        Item text = new TextItem(new Point(10,200),"Wesołych świat xdd", 20, 100,20);

        Item triangle =new Triangle(false, new Point(90,150), new Point(30,175),new Point(50,100));
        triangle.setPrecision(0.01);
    /*
        Item tri = new Triangle(false, new Point(40,40), new Point(100,200),new Point (300,450));
        tri.setPrecision(0.01);
    */

        scene.addItem(segment);
        scene.addItem(circle);
        scene.addItem(snowmanHead);
        scene.addItem(snowmanBody);
        scene.addItem(snowmanLegs);
        scene.addItem(snowmanFace);
        scene.addItem(text);
        scene.addItem(triangle);

        scene.addItem(hatBase);
        scene.addItem(hatTop);


        scene.draw();
    }
    public void boundedChristmasCard(){





    }
    public static void cubeTest()throws InterruptedException{
        Scene scene = new Scene(200,200);
        Item cube = new Cube(true, new Point(100,100),70);
        scene.addItem(cube);
        double rot=0.05;
        double sum =0.1;
        while(true){
            System.out.println(sum);
            ((Cube)cube).rotatePointsOnYAxis(rot);
            ((Cube)cube).rotatePointsOnZAxis(rot);
            ((Cube)cube).rotatePointsOnXAxis(rot);
            scene.clearScene();
            Scene.clearScreen();
            scene.draw();
            sum+=0.1;
            Thread.sleep(100);
        }

    }

    public static void tetrahedronTest() throws InterruptedException {

        //Tetrahedron tetrahedron = new Tetrahedron(true,new Point3D(50,60,-20), new Point3D(100,110,100), new Point3D(70,90,-50), new Point3D(150,110,70));
        Tetrahedron tetrahedron = new Tetrahedron(true,new Point3D(150,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50));

        AnimatedScene scene = new AnimatedScene(300,200);
        SolidRotation tetrahedronAnimation = new SolidRotation(tetrahedron,5000);
        tetrahedronAnimation.setRotationAngles(0.1,0.1,0.1);
        scene.addVideo(tetrahedronAnimation);
        scene.drawFrames();



   }
    public static  void animationTest()throws InterruptedException{
        Cube cube3 = new Cube(true, new Point(150,50),30);
        Cube cube2 = new Cube(true, new Point(100,100),50);
        Cube cube1 = new Cube(true, new Point(50,50),30);
        Tetrahedron tetrahedron1 = new Tetrahedron(true,new Point3D(150,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50) );
        SolidInterface tetrahedron2 = (new Tetrahedron(true,new Point3D(100,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50) ));
        SolidInterface decoratedTetrahedron =  new SolidBoundingBoxDecorator(tetrahedron2);

        AnimatedScene animatedScene = new AnimatedScene(400,300);
        animatedScene.setFrameRate(30);

        SolidRotation cube1Animation = new SolidRotation(cube1,2);
        SolidRotation cube2Animation = new SolidRotation(cube2,2);
        SolidRotation cube3Animation = new SolidRotation(cube3,2);
        SolidRotation tetrahydron1Rotation = new SolidRotation(tetrahedron1,2);
        SolidRotation tetrahydron2Rotation = new SolidRotation(decoratedTetrahedron,2);
        SolidTranslation cube4Animation = new SolidTranslation(cube3,2);
        SolidTranslation cube5Animation = new SolidTranslation(new Point(-1,1),cube2,2);
        SolidTranslation cube6Animation = new SolidTranslation(new Point(-1,-1),cube1,2);
        SolidTranslation tetrahydron1Translation = new SolidTranslation(new Point(1,1),tetrahedron1,2);
        SolidTranslation tetrahydron2Translation = new SolidTranslation(new Point(1,1),decoratedTetrahedron,2);





        cube1Animation.setRotationAngles(0.1,0.1,0.1);
        cube2Animation.setRotationAngles(-0.1,0.1,0.1);
        cube3Animation.setRotationAngles(-0.1,0.1,-0.1);
        tetrahydron1Rotation.setRotationAngles(0.1,0.1,0.1);
        tetrahydron2Rotation.setRotationAngles(0.1,0.1,0.1);

        VideoToASCII videoBackground = new VideoToASCII();
        videoBackground.setPosition(new Point(0,0));
        //videoBackground.loadVideo("~/Videos/simplescreenrecorder-2023-01-10_21.49.40.mp4");
        videoBackground.loadVideo("~/Videos/simplescreenrecorder-2023-01-10_23.30.22.mp4");



        animatedScene.addVideo(videoBackground);
        animatedScene.addVideo(cube1Animation);
        animatedScene.addVideo(cube2Animation);
        animatedScene.addVideo(cube3Animation);
        animatedScene.addVideo(cube4Animation);
        animatedScene.addVideo(cube5Animation);
        animatedScene.addVideo(cube6Animation);
      //  animatedScene.addVideo(tetrahydron1Translation);
       // animatedScene.addVideo(tetrahydron1Rotation);
        animatedScene.addVideo(tetrahydron2Rotation);
        animatedScene.addVideo(tetrahydron2Translation);



        animatedScene.drawFrames();

    }
    public static void testDialogScene() throws InterruptedException {
        //DialogScene scene = new DialogScene(300,300);
        //scene.addOption(new TextItem(new Point(0,0),"opcja 1",17,300,40),new Circle(false,new Point(50,50),20));
        //scene.addOption(new TextItem(new Point(0,0),"opcja 2",17,300,40),new Circle(false,new Point(70,70),30));
        //scene.selectOption(1, true);

        InteractiveDialogScene interactiveScene = new InteractiveDialogScene(300,200);
        interactiveScene.addOption(new TextItem(new Point(0,0),"Cube",17,300,40),
                new Cube(false,new Point3D(150,50,100),50 ));
        interactiveScene.addOption(new TextItem(new Point(0,0),"Tetrahedron 2",17,300,40),
                new Tetrahedron(false,new Point3D(150,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50) ) );
        interactiveScene.addOption(new TextItem(new Point(0,0),"Tetrahedron 3",17,300,40),
                new Tetrahedron(false,new Point3D(150,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50) ));

       /* interactiveScene.addOption(new TextItem(new Point(0,0),"Tetrahedron 4",17,300,40),
                new Tetrahedron(true,new Point3D(150,50,100), new Point3D(150,150,100), new Point3D(50,100,100), new Point3D(100,100,50) ) );
        */
        AnimatedScene animatedScene = new AnimatedScene(800,200);
        animatedScene.setFrameRate(30);

        VideoToASCII videoBackground = new VideoToASCII();
        videoBackground.setPosition(new Point(0,0));
        //videoBackground.setThreshold(-13200000); goose
       // videoBackground.setThreshold(-9200000); pooh, golden twerk ratio
       // videoBackground.setThreshold(-6200000); shaggy and carl macarena

        videoBackground.setThreshold(-8200000);
        //videoBackground.loadVideo("~/Videos/simplescreenrecorder-2023-01-10_21.49.40.mp4");
        videoBackground.loadVideo("~/Videos/censored.mp4");
        animatedScene.addVideo(videoBackground);

        VideoToASCII videoBackground2 = new VideoToASCII();
        videoBackground2.setThreshold(-13200000);
        videoBackground2.setPosition(new Point(0,400));
        videoBackground2.loadVideo("~/Videos/goose.mp4");

        animatedScene.addVideo(videoBackground2);



        interactiveScene.runScene();
        ArrayList<ItemInterface>itemsFromDialogScene = interactiveScene.getItems();
        ArrayList<ObjectAnimation>itemRotations = new ArrayList<>();


        for(ItemInterface item: itemsFromDialogScene){
            if(((DialogOption)item).isChosen()){
                itemRotations.add(new ComplexAnimation(new SolidBoundingBoxDecorator((SolidInterface)((DialogOption)item).getRepresentedItem()),10));
            }else{
                itemRotations.add(new ComplexAnimation((SolidInterface)((DialogOption)item).getRepresentedItem(),10));

            }
        }

        Random rand = new Random();
        SolidRotation rotation;
        SolidTranslation translation;

        for(ObjectAnimation complexAnimation: itemRotations){
            rotation =new SolidRotation((SolidInterface)complexAnimation.getItemToAnimate(),10);
            translation = new SolidTranslation(new Point(rand.nextInt(0,3)-1,rand.nextInt(0,3)-1),(SolidInterface)complexAnimation.getItemToAnimate(),10);
            rotation.setRotationAngles(rand.nextDouble(0,0.2),rand.nextDouble(0,0.2),rand.nextDouble(0,0.2));
            ((ComplexAnimation)complexAnimation).addAnimation(rotation);
            ((ComplexAnimation)complexAnimation).addAnimation(translation);
            animatedScene.addVideo(complexAnimation);

        }

        animatedScene.drawFrames();








       // scene.draw();
    }
    public static void main(String[] args) throws InterruptedException {

        //animationTest();
        //christmasCard();
        //tetrahedronTest();
        testDialogScene();
    }
}
